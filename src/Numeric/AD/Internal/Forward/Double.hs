{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
---- |
---- Copyright   :  (c) Edward Kmett 2010-2014
---- License     :  BSD3
---- Maintainer  :  ekmett@gmail.com
---- Stability   :  experimental
---- Portability :  GHC only
----
---- Unsafe and often partial combinators intended for internal usage.
----
---- Handle with care.
-------------------------------------------------------------------------------
module Numeric.AD.Internal.Forward.Double
  ( ForwardDouble(..)
  , bundle
  , unbundle
  , apply
  , bind
  , bind'
  , bindWith
  , bindWith'
  , transposeWith
  ) where

import Control.Applicative hiding ((<**>))
import Control.Monad (join)
import Data.Foldable (Foldable, toList)
import Data.Function (on)
import Data.Number.Erf
import Data.Traversable (Traversable, mapAccumL)
import Numeric.AD.Internal.Identity
import Numeric.AD.Jacobian
import Numeric.AD.Mode

data ForwardDouble a = ForwardDouble { primal, tangent :: {-# UNPACK #-} !Double }
  deriving (Read, Show)

type instance Scalar (ForwardDouble s) = Double

unbundle :: ForwardDouble s -> (Double, Double)
unbundle (ForwardDouble a da) = (a, da)
{-# INLINE unbundle #-}

bundle :: Double -> Double -> ForwardDouble s
bundle = ForwardDouble
{-# INLINE bundle #-}

apply :: (ForwardDouble s -> b) -> Double -> b
apply f a = f (bundle a 1)
{-# INLINE apply #-}

instance Mode (ForwardDouble s) where
  auto = flip ForwardDouble 0
  zero = ForwardDouble 0 0

  isKnownZero (ForwardDouble 0 0) = True
  isKnownZero _ = False

  isKnownConstant (ForwardDouble _ 0) = True
  isKnownConstant _ = False

  a *^ ForwardDouble b db = ForwardDouble (a * b) (a * db)

  ForwardDouble a da ^* b = ForwardDouble (a * b) (da * b)

  ForwardDouble a da ^/ b = ForwardDouble (a / b) (da / b)

(<+>) :: ForwardDouble s -> ForwardDouble s -> ForwardDouble s
ForwardDouble a da <+> ForwardDouble b db = ForwardDouble (a + b) (da + db)

instance Jacobian (ForwardDouble s) where
  type D (ForwardDouble s) = Id Double s

  unary f (Id dadb) (ForwardDouble b db) = ForwardDouble (f b) (dadb * db)

  lift1 f df (ForwardDouble b db) = ForwardDouble (f b) (dadb * db) where
    Id dadb = df (Id b)

  lift1_ f df (ForwardDouble b db) = ForwardDouble a da where
    a = f b
    Id da = df (Id a) (Id b) ^* db

  binary f (Id dadb) (Id dadc) (ForwardDouble b db) (ForwardDouble c dc) = ForwardDouble (f b c) $ dadb * db + dc * dadc

  lift2 f df (ForwardDouble b db) (ForwardDouble c dc) = ForwardDouble a da where
    a = f b c
    (Id dadb, Id dadc) = df (Id b) (Id c)
    da = dadb * db + dc * dadc

  lift2_ f df (ForwardDouble b db) (ForwardDouble c dc) = ForwardDouble a da where
    a = f b c
    (Id dadb, Id dadc) = df (Id a) (Id b) (Id c)
    da = dadb * db + dc * dadc

instance Eq (ForwardDouble s) where
  (==)          = on (==) primal
instance Ord (ForwardDouble s) where
  compare       = on compare primal
instance Num (ForwardDouble s) where
  fromInteger 0  = zero
  fromInteger n = auto (fromInteger n)
  (+)          = (<+>) -- binary (+) 1 1
  (-)          = binary (-) (auto 1) (auto (-1)) -- TODO: <-> ? as it is, this might be pretty bad for Tower
  (*)          = lift2 (*) (\x y -> (y, x))
  negate       = lift1 negate (const (auto (-1)))
  abs          = lift1 abs signum
  signum a     = lift1 signum (const zero) a
instance Fractional (ForwardDouble s) where
  fromRational 0 = zero
  fromRational r = auto (fromRational r)
  x / y        = x * recip y
  recip        = lift1_ recip (const . negate . join (*))
instance Floating (ForwardDouble s) where
  pi       = auto pi
  exp      = lift1_ exp const
  log      = lift1 log recip
  logBase x y = log y / log x
  sqrt     = lift1_ sqrt (\z _ -> recip (auto 2 * z))
  ForwardDouble 0 0 ** ForwardDouble a _ = ForwardDouble (0 ** a) 0
  _ ** ForwardDouble 0 0                 = ForwardDouble 1 0
  x ** ForwardDouble y 0 = lift1 (**y) (\z -> y *^ z ** Id (y - 1)) x
  x ** y                 = lift2_ (**) (\z xi yi -> (yi * z / xi, z * log xi)) x y
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
instance Enum (ForwardDouble s) where
  succ                 = lift1 succ (const 1)
  pred                 = lift1 pred (const 1)
  toEnum               = auto . toEnum
  fromEnum             = fromEnum . primal
  enumFrom a           = withPrimal a <$> enumFrom (primal a)
  enumFromTo a b       = withPrimal a <$> enumFromTo (primal a) (primal b)
  enumFromThen a b     = zipWith (fromBy a delta) [0..] $ enumFromThen (primal a) (primal b) where delta = b - a
  enumFromThenTo a b c = zipWith (fromBy a delta) [0..] $ enumFromThenTo (primal a) (primal b) (primal c) where delta = b - a
instance Real (ForwardDouble s) where
  toRational      = toRational . primal
instance RealFloat (ForwardDouble s) where
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
instance RealFrac (ForwardDouble s) where
  properFraction a = (w, a `withPrimal` pb) where
    pa = primal a
    (w, pb) = properFraction pa
  truncate = truncate . primal
  round    = round . primal
  ceiling  = ceiling . primal
  floor    = floor . primal
instance Erf (ForwardDouble s) where
  erf = lift1 erf $ \x -> (2 / sqrt pi) * exp (negate x * x)
  erfc = lift1 erfc $ \x -> ((-2) / sqrt pi) * exp (negate x * x)
  normcdf = lift1 normcdf $ \x -> ((-1) / sqrt pi) * exp (x * x * fromRational (- recip 2) / sqrt 2)
instance InvErf (ForwardDouble s) where
  inverf = lift1 inverfc $ \x -> recip $ (2 / sqrt pi) * exp (negate x * x)
  inverfc = lift1 inverfc $ \x -> recip $ negate (2 / sqrt pi) * exp (negate x * x)
  invnormcdf = lift1 invnormcdf $ \x -> recip $ ((-1) / sqrt pi) * exp (x * x * fromRational (- recip 2) / sqrt 2)

bind :: (Traversable f) => (f (ForwardDouble s) -> b) -> f Double -> f b
bind f as = snd $ mapAccumL outer (0 :: Int) as where
  outer !i _ = (i + 1, f $ snd $ mapAccumL (inner i) 0 as)
  inner !i !j a = (j + 1, if i == j then bundle a 1 else auto a)

bind' :: (Traversable f) => (f (ForwardDouble s) -> b) -> f Double -> (b, f b)
bind' f as = dropIx $ mapAccumL outer (0 :: Int, b0) as where
  outer (!i, _) _ = let b = f $ snd $ mapAccumL (inner i) (0 :: Int) as in ((i + 1, b), b)
  inner !i !j a = (j + 1, if i == j then bundle a 1 else auto a)
  b0 = f (auto <$> as)
  dropIx ((_,b),bs) = (b,bs)

bindWith :: (Traversable f) => (Double -> b -> c) -> (f (ForwardDouble s) -> b) -> f Double -> f c
bindWith g f as = snd $ mapAccumL outer (0 :: Int) as where
  outer !i a = (i + 1, g a $ f $ snd $ mapAccumL (inner i) 0 as)
  inner !i !j a = (j + 1, if i == j then bundle a 1 else auto a)

bindWith' :: (Traversable f) => (Double -> b -> c) -> (f (ForwardDouble s) -> b) -> f Double -> (b, f c)
bindWith' g f as = dropIx $ mapAccumL outer (0 :: Int, b0) as where
  outer (!i, _) a = let b = f $ snd $ mapAccumL (inner i) (0 :: Int) as in ((i + 1, b), g a b)
  inner !i !j a = (j + 1, if i == j then bundle a 1 else auto a)
  b0 = f (auto <$> as)
  dropIx ((_,b),bs) = (b,bs)

transposeWith :: (Functor f, Foldable f, Traversable g) => (b -> f a -> c) -> f (g a) -> g b -> g c
transposeWith f as = snd . mapAccumL go xss0 where
  go xss b = (tail <$> xss, f b (head <$> xss))
  xss0 = toList <$> as
