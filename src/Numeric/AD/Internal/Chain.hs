{-# LANGUAGE Rank2Types, TypeFamilies, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TemplateHaskell, UndecidableInstances, DeriveDataTypeable, GADTs, ScopedTypeVariables #-}
-- {-# OPTIONS_HADDOCK hide, prune #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Internal.Chain
-- Copyright   :  (c) Edward Kmett 2012
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Reverse-Mode Automatic Differentiation using a single tape.
--
-- This version uses @Data.Reflection@ to find and update the tape
--
-- This is asymptotically faster than using @Reverse@, which
-- is forced to reify and topologically sort the graph, but it requires
-- a fairly expensive rendezvous during construction.
--
-----------------------------------------------------------------------------

module Numeric.AD.Internal.Chain
    ( Chain(..)
    , Tape(..)
    , Head(..)
    , Cells(..)
    , reifyTape
    , partials
    , partialArray
    , partialMap
    , derivative
    , derivative'
    ) where

import Control.Monad.ST
import Data.Array.ST
import Data.Array
import Data.Array.Unsafe as Unsafe
import Data.IORef
import Data.IntMap (IntMap, fromDistinctAscList)
import Data.Proxy
import Data.Reflection
import Data.Typeable
import Language.Haskell.TH hiding (reify)
import Numeric.AD.Internal.Types
import Numeric.AD.Internal.Classes
import Numeric.AD.Internal.Identity
import Numeric.AD.Internal.Var
import Prelude hiding (mapM)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce

-- evil untyped tape
data Cells where
  Nil    :: Cells
  Unary  :: {-# UNPACK #-} !Int -> a -> Cells -> Cells
  Binary :: {-# UNPACK #-} !Int -> {-# UNPACK #-} !Int -> a -> a -> Cells -> Cells

dropCells :: Int -> Cells -> Cells
dropCells 0 xs = xs
dropCells _ Nil = Nil
dropCells n (Unary _ _ xs)      = (dropCells $! n - 1) xs
dropCells n (Binary _ _ _ _ xs) = (dropCells $! n - 1) xs

data Head = Head {-# UNPACK #-} !Int Cells

newtype Tape = Tape { getTape :: IORef Head }

un :: Int -> a -> Head -> (Head, Int)
un i di (Head r t) = (Head (r + 1) (Unary i di t), r)
{-# INLINE un #-}

bin :: Int -> Int -> a -> a -> Head -> (Head, Int)
bin i j di dj (Head r t) = (Head (r + 1) (Binary i j di dj t), r)
{-# INLINE bin #-}

modifyTape :: Reifies s Tape => proxy s -> (Head -> (Head, r)) -> IO r
modifyTape p = atomicModifyIORef (getTape (reflect p))
{-# INLINE modifyTape #-}

unarily :: forall s a. Reifies s Tape => (a -> a) -> a -> Int -> a -> Chain s a
unarily f di i b = Chain (unsafePerformIO (modifyTape (Proxy :: Proxy s) (un i di))) $! f b
{-# INLINE unarily #-}

binarily :: forall s a. Reifies s Tape => (a -> a -> a) -> a -> a -> Int -> a -> Int -> a -> Chain s a
binarily f di dj i b j c = Chain (unsafePerformIO (modifyTape (Proxy :: Proxy s) (bin i j di dj))) $! f b c
{-# INLINE binarily #-}

data Chain s a where
  Zero :: Chain s a
  Lift :: a -> Chain s a
  Chain :: {-# UNPACK #-} !Int -> a -> Chain s a
  deriving (Show, Typeable)

instance (Reifies s Tape, Lifted (Chain s)) => Mode (Chain s) where
  lift = Lift
  zero = Zero
  (<+>)  = binary (+) one one
  a *^ b = lift1 (a *) (\_ -> lift a) b
  a ^* b = lift1 (* b) (\_ -> lift b) a
  a ^/ b = lift1 (/ b) (\_ -> lift (recip b)) a

  Zero <**> y      = lift (0 ** primal y)
  _    <**> Zero   = lift 1
  x    <**> Lift y = lift1 (**y) (\z -> (y *^ z ** Id (y-1))) x
  x    <**> y      = lift2_ (**) (\z xi yi -> (yi *! z /! xi, z *! log1 xi)) x y

instance Primal (Chain s) where
    primal Zero = 0
    primal (Lift a) = a
    primal (Chain _ a) = a

instance (Reifies s Tape, Lifted (Chain s)) => Jacobian (Chain s) where
    type D (Chain s) = Id

    unary f _         (Zero)   = Lift (f 0)
    unary f _         (Lift a) = Lift (f a)
    unary f (Id dadi) (Chain i b) = unarily f dadi i b

    lift1 f df b = unary f (df (Id pb)) b
        where pb = primal b

    lift1_ f df b = unary (const a) (df (Id a) (Id pb)) b
        where pb = primal b
              a = f pb

    binary f _         _         Zero     Zero     = Lift (f 0 0)
    binary f _         _         Zero     (Lift c) = Lift (f 0 c)
    binary f _         _         (Lift b) Zero     = Lift (f b 0)
    binary f _         _         (Lift b) (Lift c) = Lift (f b c)

    binary f _         (Id dadc) Zero        (Chain i c) = unarily (f 0) dadc i c
    binary f _         (Id dadc) (Lift b)    (Chain i c) = unarily (f b) dadc i c
    binary f (Id dadb) _         (Chain i b) Zero        = unarily (`f` 0) dadb i b
    binary f (Id dadb) _         (Chain i b) (Lift c)    = unarily (`f` c) dadb i b
    binary f (Id dadb) (Id dadc) (Chain i b) (Chain j c) = binarily f dadb dadc i b j c

    lift2 f df b c = binary f dadb dadc b c
        where (dadb, dadc) = df (Id (primal b)) (Id (primal c))

    lift2_ f df b c = binary (\_ _ -> a) dadb dadc b c
        where
            pb = primal b
            pc = primal c
            a = f pb pc
            (dadb, dadc) = df (Id a) (Id pb) (Id pc)

let s = varT (mkName "s") in
  deriveLifted (classP ''Reifies [s,conT ''Tape] :) (conT ''Chain `appT` s)

derivative :: (Reifies s Tape, Num a) => AD (Chain s) a -> a
derivative = sum . partials
{-# INLINE derivative #-}

derivative' :: (Reifies s Tape, Num a) => AD (Chain s) a -> (a, a)
derivative' r = (primal r, derivative r)
{-# INLINE derivative' #-}

backPropagate :: Num a => Int -> Cells -> STArray s Int a -> ST s Int
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

{-# SPECIALIZE partials :: Reifies s Tape => AD (Chain s) Double -> [Double] #-}
partials :: forall s a. (Reifies s Tape, Num a) => AD (Chain s) a -> [a]
partials (AD Zero)        = []
partials (AD (Lift _))    = []
partials (AD (Chain k _)) = map (sensitivities !) [0..vs] where
   Head n t = unsafePerformIO $ readIORef (getTape (reflect (Proxy :: Proxy s)))
   tk = dropCells (n - k) t
   (vs,sensitivities) = runST $ do
     ss <- newArray (0, k) 0
     writeArray ss k 1
     v <- backPropagate k tk ss
     as <- Unsafe.unsafeFreeze ss
     return (v, as)

-- | Return an 'Array' of 'partials' given bounds for the variable IDs.
partialArray :: (Reifies s Tape, Num a) => (Int, Int) -> AD (Chain s) a -> Array Int a
partialArray vbounds = accumArray (+) 0 vbounds . zip [0..] . partials
{-# INLINE partialArray #-}

-- | Return an 'IntMap' of sparse partials
partialMap :: (Reifies s Tape, Num a) => AD (Chain s) a -> IntMap a
partialMap = fromDistinctAscList . zip [0..] . partials
{-# INLINE partialMap #-}

reifyTape :: Int -> (forall s. Reifies s Tape => Proxy s -> r) -> IO r
reifyTape vs k = do
  h <- newIORef (Head vs Nil)
  return (reify (Tape h) k)

instance Var (Chain s) where
    var a v = Chain v a
    varId (Chain v _) = v
    varId _ = error "varId: not a Var"
