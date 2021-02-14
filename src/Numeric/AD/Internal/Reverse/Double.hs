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
  , Head(..)
  , Cells(..)
  , reifyTape
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

import Data.Functor
import Control.Monad hiding (mapM)
import Control.Monad.ST
import Control.Monad.Trans.State
import Data.Array.ST
import Data.Array
import Data.Array.Unsafe as Unsafe
import Data.IORef
import Data.IntMap (IntMap, fromDistinctAscList, findWithDefault)
import Data.Number.Erf
import Data.Proxy
import Data.Reflection
#if __GLASGOW_HASKELL__ < 710
import Data.Traversable (Traversable, mapM)
#else
import Data.Traversable (mapM)
#endif
import Data.Typeable
import Numeric.AD.Internal.Combinators
import Numeric.AD.Internal.Identity
import Numeric.AD.Jacobian
import Numeric.AD.Mode
import Prelude hiding (mapM)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce

#ifdef HLINT
{-# ANN module "HLint: ignore Reduce duplication" #-}
#endif

-- evil untyped tape
#ifndef HLINT
data Cells where
  Nil    :: Cells
  Unary  :: {-# UNPACK #-} !Int -> {-# UNPACK #-} !Double -> !Cells -> Cells
  Binary :: {-# UNPACK #-} !Int -> {-# UNPACK #-} !Int -> {-# UNPACK #-} !Double -> {-# UNPACK #-} !Double -> !Cells -> Cells
#endif

dropCells :: Int -> Cells -> Cells
dropCells 0 xs = xs
dropCells _ Nil = Nil
dropCells n (Unary _ _ xs)      = (dropCells $! n - 1) xs
dropCells n (Binary _ _ _ _ xs) = (dropCells $! n - 1) xs

data Head = Head {-# UNPACK #-} !Int !Cells

newtype Tape = Tape { getTape :: IORef Head }

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

#ifndef HLINT
data ReverseDouble s where
  Zero :: ReverseDouble s
  Lift :: {-# UNPACK #-} !Double -> ReverseDouble s
  ReverseDouble :: {-# UNPACK #-} !Int -> {-# UNPACK #-} !Double -> ReverseDouble s
  deriving (Show, Typeable)
#endif

instance (Reifies s Tape) => Mode (ReverseDouble s) where
  type Scalar (ReverseDouble s) = Double

  isKnownZero Zero = True
  isKnownZero _    = False

  isKnownConstant ReverseDouble{} = False
  isKnownConstant _ = True

  auto = Lift
  zero = Zero
  a *^ b = lift1 (a *) (\_ -> auto a) b
  a ^* b = lift1 (* b) (\_ -> auto b) a
  a ^/ b = lift1 (/ b) (\_ -> auto (recip b)) a

(<+>) :: (Reifies s Tape) => ReverseDouble s -> ReverseDouble s -> ReverseDouble s
(<+>)  = binary (+) 1 1

(<**>) :: (Reifies s Tape) => ReverseDouble s -> ReverseDouble s -> ReverseDouble s
Zero <**> y      = auto (0 ** primal y)
_    <**> Zero   = auto 1
x    <**> Lift y = lift1 (**y) (\z -> y *^ z ** Id (y - 1)) x
x    <**> y      = lift2_ (**) (\z xi yi -> (yi * xi ** (yi - 1), z * log xi)) x y

primal :: ReverseDouble s -> Double
primal Zero = 0
primal (Lift a) = a
primal (ReverseDouble _ a) = a

instance (Reifies s Tape) => Jacobian (ReverseDouble s) where
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


instance (Reifies s Tape) => Eq (ReverseDouble s) where
  a == b = primal a == primal b

instance (Reifies s Tape) => Ord (ReverseDouble s) where
  compare a b = compare (primal a) (primal b)

instance (Reifies s Tape) => Num (ReverseDouble s) where
  fromInteger 0  = zero
  fromInteger n = auto (fromInteger n)
  (+)          = (<+>) -- binary (+) 1 1
  (-)          = binary (-) (auto 1) (auto (-1)) -- TODO: <-> ? as it is, this might be pretty bad for Tower
  (*)          = lift2 (*) (\x y -> (y, x))
  negate       = lift1 negate (const (auto (-1)))
  abs          = lift1 abs signum
  signum a     = lift1 signum (const zero) a

instance (Reifies s Tape) => Fractional (ReverseDouble s) where
  fromRational 0 = zero
  fromRational r = auto (fromRational r)
  x / y        = x * recip y
  recip        = lift1_ recip (const . negate . join (*))

instance (Reifies s Tape) => Floating (ReverseDouble s) where
  pi       = auto pi
  exp      = lift1_ exp const
  log      = lift1 log recip
  logBase x y = log y / log x
  sqrt     = lift1_ sqrt (\z _ -> recip (auto 2 * z))
  (**)     = (<**>)
  --x ** y
  --   | isKnownZero y     = 1
  --   | isKnownConstant y, y' <- primal y = lift1 (** y') ((y'*) . (**(y'-1))) x
  --   | otherwise         = lift2_ (**) (\z xi yi -> (yi * z / xi, z * log1 xi)) x y
  sin      = lift1 sin cos
  cos      = lift1 cos $ negate . sin
  tan      = lift1 tan $ recip . join (*) . cos
  asin     = lift1 asin $ \x -> recip (sqrt (auto 1 - join (*) x))
  acos     = lift1 acos $ \x -> negate (recip (sqrt (1 - join (*) x)))
  atan     = lift1 atan $ \x -> recip (1 + join (*) x)
  sinh     = lift1 sinh cosh
  cosh     = lift1 cosh sinh
  tanh     = lift1 tanh $ recip . join (*) . cosh
  asinh    = lift1 asinh $ \x -> recip (sqrt (1 + join (*) x))
  acosh    = lift1 acosh $ \x -> recip (sqrt (join (*) x - 1))
  atanh    = lift1 atanh $ \x -> recip (1 - join (*) x)

instance (Reifies s Tape) => Enum (ReverseDouble s) where
  succ             = lift1 succ (const 1)
  pred             = lift1 pred (const 1)
  toEnum           = auto . toEnum
  fromEnum a       = fromEnum (primal a)
  enumFrom a       = withPrimal a <$> enumFrom (primal a)
  enumFromTo a b   = withPrimal a <$> enumFromTo (primal a) (primal b)
  enumFromThen a b = zipWith (fromBy a delta) [0..] $ enumFromThen (primal a) (primal b) where delta = b - a
  enumFromThenTo a b c = zipWith (fromBy a delta) [0..] $ enumFromThenTo (primal a) (primal b) (primal c) where delta = b - a

instance (Reifies s Tape) => Real (ReverseDouble s) where
  toRational      = toRational . primal

instance (Reifies s Tape) => RealFloat (ReverseDouble s) where
  floatRadix      = floatRadix . primal
  floatDigits     = floatDigits . primal
  floatRange      = floatRange . primal
  decodeFloat     = decodeFloat . primal
  encodeFloat m e = auto (encodeFloat m e)
  isNaN           = isNaN . primal
  isInfinite      = isInfinite . primal
  isDenormalized  = isDenormalized . primal
  isNegativeZero  = isNegativeZero . primal
  isIEEE          = isIEEE . primal
  exponent = exponent
  scaleFloat n = unary (scaleFloat n) (scaleFloat n 1)
  significand x =  unary significand (scaleFloat (- floatDigits x) 1) x
  atan2 = lift2 atan2 $ \vx vy -> let r = recip (join (*) vx + join (*) vy) in (vy * r, negate vx * r)

instance (Reifies s Tape) => RealFrac (ReverseDouble s) where
  properFraction a = (w, a `withPrimal` pb) where
    pa = primal a
    (w, pb) = properFraction pa
  truncate = truncate . primal
  round    = round . primal
  ceiling  = ceiling . primal
  floor    = floor . primal

instance (Reifies s Tape) => Erf (ReverseDouble s) where
  erf = lift1 erf $ \x -> (2 / sqrt pi) * exp (negate x * x)
  erfc = lift1 erfc $ \x -> ((-2) / sqrt pi) * exp (negate x * x)
  normcdf = lift1 normcdf $ \x -> recip (sqrt (2 * pi)) * exp (- x * x / 2)

instance (Reifies s Tape) => InvErf (ReverseDouble s) where
  inverf = lift1_ inverf $ \x _ -> sqrt pi / 2 * exp (x * x)
  inverfc = lift1_ inverfc $ \x _ -> negate (sqrt pi / 2) * exp (x * x)
  invnormcdf = lift1_ invnormcdf $ \x _ -> sqrt (2 * pi) * exp (x * x / 2)

-- | Helper that extracts the derivative of a chain when the chain was constructed with 1 variable.
derivativeOf :: (Reifies s Tape) => Proxy s -> ReverseDouble s -> Double
derivativeOf _ = sum . partials
{-# INLINE derivativeOf #-}

-- | Helper that extracts both the primal and derivative of a chain when the chain was constructed with 1 variable.
derivativeOf' :: (Reifies s Tape) => Proxy s -> ReverseDouble s -> (Double, Double)
derivativeOf' p r = (primal r, derivativeOf p r)
{-# INLINE derivativeOf' #-}

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
partials :: forall s. (Reifies s Tape) => ReverseDouble s -> [Double]
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

-- | Return an 'Array' of 'partials' given bounds for the variable IDs.
partialArrayOf :: (Reifies s Tape) => Proxy s -> (Int, Int) -> ReverseDouble s -> Array Int Double
partialArrayOf _ vbounds = accumArray (+) 0 vbounds . zip [0..] . partials
{-# INLINE partialArrayOf #-}

-- | Return an 'IntMap' of sparse partials
partialMapOf :: (Reifies s Tape) => Proxy s -> ReverseDouble s-> IntMap Double
partialMapOf _ = fromDistinctAscList . zip [0..] . partials
{-# INLINE partialMapOf #-}

-- | Construct a tape that starts with @n@ variables.
reifyTape :: Int -> (forall s. Reifies s Tape => Proxy s -> r) -> r
reifyTape vs k = unsafePerformIO $ do
  h <- newIORef (Head vs Nil)
  return (reify (Tape h) k)
{-# NOINLINE reifyTape #-}

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

unbindWith :: (Functor f) => (Double -> b -> c) -> f (ReverseDouble s) -> Array Int b -> f c
unbindWith f xs ys = fmap (\v -> f (primal v) (ys ! varId v)) xs

unbindMap :: (Functor f) => f (ReverseDouble s) -> IntMap Double -> f Double
unbindMap xs ys = fmap (\v -> findWithDefault 0 (varId v) ys) xs

unbindMapWithDefault :: (Functor f) => b -> (Double -> b -> c) -> f (ReverseDouble s) -> IntMap b -> f c
unbindMapWithDefault z f xs ys = fmap (\v -> f (primal v) $ findWithDefault z (varId v) ys) xs
