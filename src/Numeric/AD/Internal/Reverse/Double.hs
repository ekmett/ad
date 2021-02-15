{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_HADDOCK not-home #-}

module Numeric.AD.Internal.Reverse.Double
  ( ReverseDouble(..)
  , Tape(..)
  , reifyTape
  , reifyTypeableTape
  , partials
  , partialArrayOf
  , partialMapOf
  , derivativeOf
  , derivativeOf'
  , bind
  , unbind
  , unbindMap
  , unbindWith
  , unbindMapWithDefault
  , var
  , varId
  , primal
  ) where

#ifdef AD_FFI
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import qualified Foreign.Marshal.Array as MA
import qualified Foreign.Marshal.Alloc as MA
#else
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unsafe as Unsafe
import Data.IORef
import Unsafe.Coerce
#endif

import Data.Functor
import Control.Monad hiding (mapM)
import Control.Monad.Trans.State
import Data.Array
import Data.IntMap (IntMap, fromDistinctAscList, findWithDefault)
import Data.Number.Erf
import Data.Proxy
import Data.Reflection
import Data.Traversable (mapM)
import Data.Typeable
import Numeric.AD.Internal.Combinators
import Numeric.AD.Internal.Identity
import Numeric.AD.Jacobian
import Numeric.AD.Mode
import Prelude hiding (mapM)
import System.IO.Unsafe (unsafePerformIO)

#ifdef AD_FFI

newtype Tape = Tape { getTape :: ForeignPtr Tape }

foreign import ccall unsafe "tape_alloc" c_tape_alloc :: CInt -> CInt -> IO (Ptr Tape)
foreign import ccall unsafe "tape_push" c_tape_push :: Ptr Tape -> CInt -> CInt -> Double -> Double -> IO Int
foreign import ccall unsafe "tape_backPropagate" c_tape_backPropagate :: Ptr Tape -> CInt -> Ptr Double -> IO ()
foreign import ccall unsafe "tape_variables" c_tape_variables :: Ptr Tape -> IO CInt
foreign import ccall unsafe "&tape_free" c_ref_tape_free :: FinalizerPtr Tape

pushTape :: Reifies s Tape => p s -> Int -> Int -> Double -> Double -> IO Int
pushTape p i1 i2 d1 d2 = do
  withForeignPtr (getTape (reflect p)) $ \tape -> 
    c_tape_push tape (fromIntegral i1) (fromIntegral i2) d1 d2
{-# INLINE pushTape #-}

-- | Extract the partials from the current chain for a given AD variable.
partials :: forall s. (Reifies s Tape) => ReverseDouble s -> [Double]
partials Zero        = []
partials (Lift _)    = []
partials (ReverseDouble k _) = unsafePerformIO $
  withForeignPtr (getTape (reflect (Proxy :: Proxy s))) $ \tape -> do
    l <- fromIntegral <$> c_tape_variables tape
    arr <- MA.mallocArray l
    c_tape_backPropagate tape (fromIntegral k) arr
    ps <- MA.peekArray l arr
    MA.free arr
    return ps
{-# INLINE partials #-}

newTape :: Int -> IO Tape
newTape vs = do
  p <- c_tape_alloc (fromIntegral vs) (4 * 1024)
  Tape <$> newForeignPtr c_ref_tape_free p

-- | Construct a tape that starts with @n@ variables.
reifyTape :: Int -> (forall s. Reifies s Tape => Proxy s -> r) -> r
reifyTape vs k = unsafePerformIO $ fmap (\t -> reify t k) (newTape vs)
{-# NOINLINE reifyTape #-}

-- | Construct a tape that starts with @n@ variables.
reifyTypeableTape :: Int -> (forall s. (Reifies s Tape, Typeable s) => Proxy s -> r) -> r
reifyTypeableTape vs k = unsafePerformIO $ fmap (\t -> reifyTypeable t k) (newTape vs)
{-# NOINLINE reifyTypeableTape #-}

-- | This is used to create a new entry on the chain given a unary function, its derivative with respect to its input,
-- the variable ID of its input, and the value of its input. Used by 'unary' and 'binary' internally.
unarily :: forall s. Reifies s Tape => (Double -> Double) -> Double -> Int -> Double -> ReverseDouble s
unarily f di i b = ReverseDouble (unsafePerformIO (pushTape (Proxy :: Proxy s) i 0 di 0.0)) $! f b
{-# INLINE unarily #-}

-- | This is used to create a new entry on the chain given a binary function, its derivatives with respect to its inputs,
-- their variable IDs and values. Used by 'binary' internally.
binarily :: forall s. Reifies s Tape => (Double -> Double -> Double) -> Double -> Double -> Int -> Double -> Int -> Double -> ReverseDouble s
binarily f di dj i b j c = ReverseDouble (unsafePerformIO (pushTape (Proxy :: Proxy s) i j di dj)) $! f b c
{-# INLINE binarily #-}

#else

data Cells where
  Nil    :: Cells
  Unary  :: {-# UNPACK #-} !Int -> {-# UNPACK #-} !Double -> !Cells -> Cells
  Binary :: {-# UNPACK #-} !Int -> {-# UNPACK #-} !Int -> {-# UNPACK #-} !Double -> {-# UNPACK #-} !Double -> !Cells -> Cells

dropCells :: Int -> Cells -> Cells
dropCells 0 xs = xs
dropCells _ Nil = Nil
dropCells n (Unary _ _ xs)      = (dropCells $! n - 1) xs
dropCells n (Binary _ _ _ _ xs) = (dropCells $! n - 1) xs

data Head = Head {-# UNPACK #-} !Int !Cells

newtype Tape = Tape { getTape :: IORef Head }

-- | Used internally to push sensitivities down the chain.
backPropagate :: Int -> Cells -> STArray s Int Double -> ST s Int
backPropagate k Nil _ = return k
backPropagate k (Unary i g xs) ss = do
  da <- readArray ss k
  db <- readArray ss i
  writeArray ss i $! db + unsafeCoerce g*da
  (backPropagate $! k - 1) xs ss
backPropagate k (Binary i j g h xs) ss = do
  da <- readArray ss k
  db <- readArray ss i
  writeArray ss i $! db + unsafeCoerce g*da
  dc <- readArray ss j
  writeArray ss j $! dc + unsafeCoerce h*da
  (backPropagate $! k - 1) xs ss

-- | Extract the partials from the current chain for a given AD variable.
partials :: forall s. Reifies s Tape => ReverseDouble s -> [Double]
partials Zero        = []
partials (Lift _)    = []
partials (ReverseDouble k _) = map (sensitivities !) [0..vs] where
  Head n t = unsafePerformIO $ readIORef (getTape (reflect (Proxy :: Proxy s)))
  tk = dropCells (n - k) t
  (vs,sensitivities) = runST $ do
    ss <- newArray (0, k) 0
    writeArray ss k 1
    v <- backPropagate k tk ss
    as <- Unsafe.unsafeFreeze ss
    return (v, as)

-- | Construct a tape that starts with @n@ variables.
reifyTape :: Int -> (forall s. Reifies s Tape => Proxy s -> r) -> r
reifyTape vs k = unsafePerformIO $ do
  h <- newIORef (Head vs Nil)
  return (reify (Tape h) k)
{-# NOINLINE reifyTape #-}

-- | Construct a tape that starts with @n@ variables.
reifyTypeableTape :: Int -> (forall s. (Reifies s Tape, Typeable s) => Proxy s -> r) -> r
reifyTypeableTape vs k = unsafePerformIO $ do
  h <- newIORef (Head vs Nil)
  return (reifyTypeable (Tape h) k)
{-# NOINLINE reifyTypeableTape #-}

un :: Int -> Double -> Head -> (Head, Int)
un i di (Head r t) = h `seq` r' `seq` (h, r') where
  r' = r + 1
  h = Head r' (Unary i di t)
{-# INLINE un #-}

bin :: Int -> Int -> Double -> Double -> Head -> (Head, Int)
bin i j di dj (Head r t) = h `seq` r' `seq` (h, r') where
  r' = r + 1
  h = Head r' (Binary i j di dj t)
{-# INLINE bin #-}

modifyTape :: Reifies s Tape => p s -> (Head -> (Head, r)) -> IO r
modifyTape p = atomicModifyIORef (getTape (reflect p))
{-# INLINE modifyTape #-}

-- | This is used to create a new entry on the chain given a unary function, its derivative with respect to its input,
-- the variable ID of its input, and the value of its input. Used by 'unary' and 'binary' internally.
unarily :: forall s. Reifies s Tape => (Double -> Double) -> Double -> Int -> Double -> ReverseDouble s
unarily f di i b = ReverseDouble (unsafePerformIO (modifyTape (Proxy :: Proxy s) (un i di))) $! f b
{-# INLINE unarily #-}

-- | This is used to create a new entry on the chain given a binary function, its derivatives with respect to its inputs,
-- their variable IDs and values. Used by 'binary' internally.
binarily :: forall s. Reifies s Tape => (Double -> Double -> Double) -> Double -> Double -> Int -> Double -> Int -> Double -> ReverseDouble s
binarily f di dj i b j c = ReverseDouble (unsafePerformIO (modifyTape (Proxy :: Proxy s) (bin i j di dj))) $! f b c
{-# INLINE binarily #-}

#endif

data ReverseDouble s where
  Zero :: ReverseDouble s
  Lift :: {-# UNPACK #-} !Double -> ReverseDouble s
  ReverseDouble :: {-# UNPACK #-} !Int -> {-# UNPACK #-} !Double -> ReverseDouble s
  deriving (Show, Typeable)

instance Reifies s Tape => Mode (ReverseDouble s) where
  type Scalar (ReverseDouble s) = Double

  isKnownZero Zero = True
  isKnownZero (Lift 0) = True
  isKnownZero _    = False

  isKnownConstant ReverseDouble{} = False
  isKnownConstant _ = True

  auto = Lift
  zero = Zero
  a *^ b = lift1 (a *) (\_ -> auto a) b
  a ^* b = lift1 (* b) (\_ -> auto b) a
  a ^/ b = lift1 (/ b) (\_ -> auto (recip b)) a

(<+>) :: Reifies s Tape => ReverseDouble s -> ReverseDouble s -> ReverseDouble s
(<+>)  = binary (+) 1 1

primal :: ReverseDouble s -> Double
primal Zero = 0
primal (Lift a) = a
primal (ReverseDouble _ a) = a

instance Reifies s Tape => Jacobian (ReverseDouble s) where
  type D (ReverseDouble s) = Id Double

  unary f _          Zero    = Lift (f 0)
  unary f _         (Lift a) = Lift (f a)
  unary f (Id dadi) (ReverseDouble i b) = unarily f dadi i b

  lift1 f df b = unary f (df (Id pb)) b where
    pb = primal b

  lift1_ f df b = unary (const a) (df (Id a) (Id pb)) b where
    pb = primal b
    a = f pb

  binary f _         _         Zero     Zero     = Lift (f 0 0)
  binary f _         _         Zero     (Lift c) = Lift (f 0 c)
  binary f _         _         (Lift b) Zero     = Lift (f b 0)
  binary f _         _         (Lift b) (Lift c) = Lift (f b c)

  binary f _         (Id dadc) Zero        (ReverseDouble i c) = unarily (f 0) dadc i c
  binary f _         (Id dadc) (Lift b)    (ReverseDouble i c) = unarily (f b) dadc i c
  binary f (Id dadb) _         (ReverseDouble i b) Zero        = unarily (`f` 0) dadb i b
  binary f (Id dadb) _         (ReverseDouble i b) (Lift c)    = unarily (`f` c) dadb i b
  binary f (Id dadb) (Id dadc) (ReverseDouble i b) (ReverseDouble j c) = binarily f dadb dadc i b j c

  lift2 f df b c = binary f dadb dadc b c where
    (dadb, dadc) = df (Id (primal b)) (Id (primal c))

  lift2_ f df b c = binary (\_ _ -> a) dadb dadc b c where
    pb = primal b
    pc = primal c
    a = f pb pc
    (dadb, dadc) = df (Id a) (Id pb) (Id pc)

mul :: Reifies s Tape => ReverseDouble s -> ReverseDouble s -> ReverseDouble s
mul = lift2 (*) (\x y -> (y, x))

#define BODY1(x) Reifies s Tape =>
#define BODY2(x,y) Reifies s Tape =>
#define HEAD (ReverseDouble s)
#define NO_Bounded
#include "instances.h"

-- | Helper that extracts the derivative of a chain when the chain was constructed with 1 variable.
derivativeOf :: Reifies s Tape => Proxy s -> ReverseDouble s -> Double
derivativeOf _ = sum . partials
{-# INLINE derivativeOf #-}

-- | Helper that extracts both the primal and derivative of a chain when the chain was constructed with 1 variable.
derivativeOf' :: Reifies s Tape => Proxy s -> ReverseDouble s -> (Double, Double)
derivativeOf' p r = (primal r, derivativeOf p r)
{-# INLINE derivativeOf' #-}


-- | Return an 'Array' of 'partials' given bounds for the variable IDs.
partialArrayOf :: Reifies s Tape => Proxy s -> (Int, Int) -> ReverseDouble s -> Array Int Double
partialArrayOf _ vbounds = accumArray (+) 0 vbounds . zip [0..] . partials
{-# INLINE partialArrayOf #-}

-- | Return an 'IntMap' of sparse partials
partialMapOf :: Reifies s Tape => Proxy s -> ReverseDouble s-> IntMap Double
partialMapOf _ = fromDistinctAscList . zip [0..] . partials
{-# INLINE partialMapOf #-}

var :: Double -> Int -> ReverseDouble s
var a v = ReverseDouble v a

varId :: ReverseDouble s -> Int
varId (ReverseDouble v _) = v
varId _ = error "varId: not a Var"

bind :: Traversable f => f Double -> (f (ReverseDouble s), (Int,Int))
bind xs = (r,(0,hi)) where
  (r,hi) = runState (mapM freshVar xs) 0
  freshVar a = state $ \s -> let s' = s + 1 in s' `seq` (var a s, s')

unbind :: Functor f => f (ReverseDouble s) -> Array Int Double -> f Double
unbind xs ys = fmap (\v -> ys ! varId v) xs

unbindWith :: Functor f => (Double -> b -> c) -> f (ReverseDouble s) -> Array Int b -> f c
unbindWith f xs ys = fmap (\v -> f (primal v) (ys ! varId v)) xs

unbindMap :: Functor f => f (ReverseDouble s) -> IntMap Double -> f Double
unbindMap xs ys = fmap (\v -> findWithDefault 0 (varId v) ys) xs

unbindMapWithDefault :: Functor f => b -> (Double -> b -> c) -> f (ReverseDouble s) -> IntMap b -> f c
unbindMapWithDefault z f xs ys = fmap (\v -> f (primal v) $ findWithDefault z (varId v) ys) xs
