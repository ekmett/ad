{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Internal.Classes
-- Copyright   :  (c) Edward Kmett 2010-2014
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------

module Numeric.AD.Internal.Classes
  (
  -- * AD modes
    Mode(..)
  , one
  , negOne
  -- * Automatically Deriving AD
  , Lifted(..)
  , Jacobian(..)
  , Primal(..)
  , deriveNumeric
  , Scalar

  , discrete1
  , discrete2
  , discrete3
  , withPrimal
  , fromBy
  ) where

import Control.Applicative ((<$>), pure)
import Control.Monad
import Data.Number.Erf
import Data.Proxy
import Language.Haskell.TH.Syntax

type family Scalar t

class Lifted g where
  liftBounded    :: (a ~ Scalar g, Bounded a, Num a)  => p g -> (Bounded g    => r) -> r
  liftEnum       :: (a ~ Scalar g, Enum    a, Num a)  => p g -> (Enum g       => r) -> r
  liftEq         :: (a ~ Scalar g, Eq      a, Num a)  => p g -> (Eq g         => r) -> r
  liftOrd        :: (a ~ Scalar g, Ord     a, Num a)  => p g -> (Ord g        => r) -> r
  liftNum        :: (a ~ Scalar g, Num a)             => p g -> (Num g        => r) -> r
  liftFractional :: (a ~ Scalar g, Fractional a)      => p g -> (Fractional g => r) -> r
  liftFloating   :: (a ~ Scalar g, Floating a)        => p g -> (Floating g   => r) -> r
  liftRealFloat  :: (a ~ Scalar g, RealFloat a)       => p g -> (RealFloat g  => r) -> r
  liftRealFrac   :: (a ~ Scalar g, RealFrac a)        => p g -> (RealFrac g   => r) -> r
  liftReal       :: (a ~ Scalar g, Real a)            => p g -> (Real g       => r) -> r
  liftErf        :: (a ~ Scalar g, Erf a)             => p g -> (Erf g        => r) -> r
  liftInvErf     :: (a ~ Scalar g, InvErf a)          => p g -> (InvErf g     => r) -> r
  liftMode       :: (a ~ Scalar g, Mode a, Num a)     => p g -> (Mode g       => r) -> r
  liftPrimal     :: (a ~ Scalar g, Primal a, Num a)   => p g -> (Primal g     => r) -> r
  liftJacobian   :: (a ~ Scalar g, Jacobian a, Num a) => p g -> (Jacobian g   => r) -> r
  -- liftScalar     ::                 p (f a) -> (Scalar (f a) ~ a => r) -> r

  liftedBounded :: forall a. (a ~ Scalar g, Bounded a, Num a) => (Bounded g => g) -> g
  liftedBounded = liftBounded (Proxy :: Proxy g)

  liftedNum :: forall a. (Num a, a ~ Scalar g) => (Num g => g) -> g
  liftedNum = liftNum (Proxy :: Proxy g)

  liftedEnum :: forall a. (a ~ Scalar g, Enum a, Num a) => (Enum g => g) -> g
  liftedEnum = liftEnum (Proxy :: Proxy g)

  liftedFractional :: forall a. (a ~ Scalar g, Fractional a) => (Fractional g => g) -> g
  liftedFractional = liftFractional (Proxy :: Proxy g)

  liftedFloating :: forall a. (a ~ Scalar g, Floating a) => (Floating g => g) -> g
  liftedFloating = liftFloating (Proxy :: Proxy g)

  liftedRealFloat :: forall a. (a ~ Scalar g, RealFloat a) => (RealFloat g => g) -> g
  liftedRealFloat = liftRealFloat (Proxy :: Proxy g)

  liftedErf :: forall a. (a ~ Scalar g, Erf a) => (Erf g => g) -> g
  liftedErf = liftErf (Proxy :: Proxy g)

  liftedInvErf :: forall a. (a ~ Scalar g, InvErf a) => (InvErf g => g) -> g
  liftedInvErf = liftInvErf (Proxy :: Proxy g)

  liftedMode :: forall a. (a ~ Scalar g, Mode a, Num a) => (Mode g => g) -> g
  liftedMode = liftMode (Proxy :: Proxy g)

infixr 6 <+>
infixr 7 *^
infixl 7 ^*
infixr 7 ^/
infixr 8 <**>

class (Num (Scalar t)) => Mode t where
  -- | allowed to return False for items with a zero derivative, but we'll give more NaNs than strictly necessary
  isKnownConstant :: t -> Bool
  isKnownConstant _ = False

  -- | allowed to return False for zero, but we give more NaN's than strictly necessary then
  isKnownZero :: t -> Bool
  isKnownZero _ = False

  -- | Embed a constant
  auto  :: Scalar t -> t

  -- | Vector sum
  (<+>) :: Num (Scalar t) => t -> t -> t

  -- | Scalar-vector multiplication
  (*^) :: Scalar t -> t -> t

  -- | Vector-scalar multiplication
  (^*) :: t -> Scalar t -> t

  -- | Scalar division
  (^/) :: (Num t, Fractional (Scalar t)) => t -> Scalar t -> t

  -- | Exponentiation, this should be overloaded if you can figure out anything about what is constant!
  (<**>) :: (Floating (Scalar t)) => t -> t -> t

  -- default (<**>) :: (Jacobian t, Floating (D t), Floating (Scalar t)) => t -> t -> t
  -- x <**> y = lift2_ (**) (\z xi yi -> (yi * z / xi, z * log xi)) x y

  -- | > 'zero' = 'lift' 0
  zero :: t

#ifndef HLINT
  default (*^) :: Num t => Scalar t -> t -> t
  a *^ b = auto a * b
  default (^*) :: Num t => t -> Scalar t -> t
  a ^* b = a * auto b
#endif

  a ^/ b = a ^* recip b

  zero = auto 0

one :: Mode t => t
one = auto 1
{-# INLINE one #-}

negOne :: Mode t => t
negOne = auto (-1)
{-# INLINE negOne #-}

-- | 'Primal' is used by 'deriveMode' but is not exposed
-- via the 'Mode' class to prevent its abuse by end users
-- via the AD data type.
--
-- It provides direct access to the result, stripped of its derivative information,
-- but this is unsafe in general as (auto . primal) would discard derivative
-- information. The end user is protected from accidentally using this function
-- by the universal quantification on the various combinators we expose.

class Primal t where
  primal :: t -> Scalar t

-- | 'Jacobian' is used by 'deriveMode' but is not exposed
-- via 'Mode' to prevent its abuse by end users
-- via the 'AD' data type.
class (Mode t, Mode (D t), Num (D t)) => Jacobian t where
  type D t :: *

  unary  :: (Scalar t -> Scalar t) -> D t -> t -> t
  lift1  :: (Scalar t -> Scalar t) -> (D t -> D t) -> t -> t
  lift1_ :: (Scalar t -> Scalar t) -> (D t -> D t -> D t) -> t -> t

  binary :: (Scalar t -> Scalar t -> Scalar t) -> D t -> D t -> t -> t -> t
  lift2  :: (Scalar t -> Scalar t -> Scalar t) -> (D t -> D t -> (D t, D t)) -> t -> t -> t
  lift2_ :: (Scalar t -> Scalar t -> Scalar t) -> (D t -> D t -> D t -> (D t, D t)) -> t -> t -> t

withPrimal :: (Jacobian t, Scalar t ~ Scalar (D t)) => t -> Scalar t -> t
withPrimal t a = unary (const a) one t
{-# INLINE withPrimal #-}

fromBy :: (Jacobian t, Scalar t ~ Scalar (D t)) => t -> t -> Int -> Scalar t -> t
fromBy a delta n x = binary (\_ _ -> x) one (fromIntegral n) a delta

discrete1 :: Primal t => (Scalar t -> c) -> t -> c
discrete1 f x = f (primal x)
{-# INLINE discrete1 #-}

discrete2 :: Primal t => (Scalar t -> Scalar t -> c) -> t -> t -> c
discrete2 f x y = f (primal x) (primal y)
{-# INLINE discrete2 #-}

discrete3 :: Primal t => (Scalar t -> Scalar t -> Scalar t -> d) -> t -> t -> t -> d
discrete3 f x y z = f (primal x) (primal y) (primal z)
{-# INLINE discrete3 #-}

-- | @'deriveNumeric g s@ provides the following instances:
--
-- > instance ('Num' a, 'Enum' a) => 'Enum' ($g a s)
-- > instance ('Num' a, 'Eq' a) => 'Eq' ($g a s)
-- > instance ('Num' a, 'Ord' a) => 'Ord' ($g a s)
-- > instance ('Num' a, 'Bounded' a) => 'Bounded' ($g a s)
--
-- > instance ('Num' a) => 'Num' ($g a s)
-- > instance ('Fractional' a) => 'Fractional' ($g a s)
-- > instance ('Floating' a) => 'Floating' ($g a s)
-- > instance ('RealFloat' a) => 'RealFloat' ($g a s)
-- > instance ('RealFrac' a) => 'RealFrac' ($g a s)
-- > instance ('Real' a) => 'Real' ($g a s)
deriveNumeric :: ([Pred] -> [Pred]) -> Type -> Type -> Q [Dec]
deriveNumeric f tCon s' = map fudgeCxt <$> lifted
  where
    t = pure tCon
    s = pure s'
    fudgeCxt (InstanceD cxt typ dec) = InstanceD (f cxt) typ dec
    fudgeCxt _ = error "Numeric.AD.Internal.Classes.deriveNumeric_fudgeCxt: Not InstanceD"
    lifted = [d|
      instance Lifted ($t a $s) where
        liftBounded    _ a = a
        liftEnum       _ a = a
        liftEq         _ a = a
        liftOrd        _ a = a
        liftNum        _ a = a
        liftFractional _ a = a
        liftFloating   _ a = a
        liftRealFloat  _ a = a
        liftRealFrac   _ a = a
        liftReal       _ a = a
        liftErf        _ a = a
        liftInvErf     _ a = a
        liftMode       _ a = a
        liftPrimal     _ a = a
        liftJacobian   _ a = a
      instance (Eq a, Num a) => Eq ($t a $s) where
        (==)          = discrete2 (==)
      instance (Ord a, Num a) => Ord ($t a $s) where
        compare       = discrete2 compare
      instance (Bounded a, Num a) => Bounded ($t a $s) where
        maxBound      = auto maxBound
        minBound      = auto minBound
      instance Num a => Num ($t a $s) where
        fromInteger 0  = zero
        fromInteger n = auto (fromInteger n)
        (+)          = (<+>) -- binary (+) one one
        (-)          = binary (-) (auto 1) (auto (-1)) -- TODO: <-> ? as it is, this might be pretty bad for Tower
        (*)          = lift2 (*) (\x y -> (y, x))
        negate       = lift1 negate (const (auto (-1)))
        abs          = lift1 abs signum
        signum a     = lift1 signum (const zero) a
      instance Fractional a => Fractional ($t a $s) where
        fromRational 0 = zero
        fromRational r = auto (fromRational r)
        x / y        = x * recip y
        recip        = lift1_ recip (const . negate . join (*))
      instance Floating a => Floating ($t a $s) where
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
        acos     = lift1 acos $ \x -> negate (recip (sqrt (one - join (*) x)))
        atan     = lift1 atan $ \x -> recip (one + join (*) x)
        sinh     = lift1 sinh cosh
        cosh     = lift1 cosh sinh
        tanh     = lift1 tanh $ recip . join (*) . cosh
        asinh    = lift1 asinh $ \x -> recip (sqrt (one + join (*) x))
        acosh    = lift1 acosh $ \x -> recip (sqrt (join (*) x - one))
        atanh    = lift1 atanh $ \x -> recip (one - join (*) x)
      instance (Num a, Enum a) => Enum ($t a $s) where
        succ                 = lift1 succ (const one)
        pred                 = lift1 pred (const one)
        toEnum               = auto . toEnum
        fromEnum             = discrete1 fromEnum
        enumFrom a           = withPrimal a <$> enumFrom (primal a)
        enumFromTo a b       = withPrimal a <$> discrete2 enumFromTo a b
        enumFromThen a b     = zipWith (fromBy a delta) [0..] $ discrete2 enumFromThen a b where delta = b - a
        enumFromThenTo a b c = zipWith (fromBy a delta) [0..] $ discrete3 enumFromThenTo a b c where delta = b - a
      instance Real a => Real ($t a $s) where
        toRational      = discrete1 toRational
      instance RealFloat a => RealFloat ($t a $s) where
        floatRadix      = discrete1 floatRadix
        floatDigits     = discrete1 floatDigits
        floatRange      = discrete1 floatRange
        decodeFloat     = discrete1 decodeFloat
        encodeFloat m e = auto (encodeFloat m e)
        isNaN           = discrete1 isNaN
        isInfinite      = discrete1 isInfinite
        isDenormalized  = discrete1 isDenormalized
        isNegativeZero  = discrete1 isNegativeZero
        isIEEE          = discrete1 isIEEE
        exponent = exponent
        scaleFloat n = unary (scaleFloat n) (scaleFloat n one)
        significand x =  unary significand (scaleFloat (- floatDigits x) one) x
        atan2 = lift2 atan2 $ \vx vy -> let r = recip (join (*) vx + join (*) vy) in (vy * r, negate vx * r)
      instance RealFrac a => RealFrac ($t a $s) where
        properFraction a = (w, a `withPrimal` pb) where
            pa = primal a
            (w, pb) = properFraction pa
        truncate = discrete1 truncate
        round    = discrete1 round
        ceiling  = discrete1 ceiling
        floor    = discrete1 floor
      instance Erf a => Erf ($t a $s) where
        erf = lift1 erf $ \x -> (fromInteger 2 / sqrt pi) * exp (negate x * x)
        erfc = lift1 erfc $ \x -> (fromInteger (-2) / sqrt pi) * exp (negate x * x)
        normcdf = lift1 normcdf $ \x -> (fromInteger (-1) / sqrt pi) * exp (x * x * fromRational (- recip 2) / sqrt (fromInteger 2))
      instance InvErf a => InvErf ($t a $s) where
        inverf = lift1 inverfc $ \x -> recip $ (fromInteger 2 / sqrt pi) * exp (negate x * x)
        inverfc = lift1 inverfc $ \x -> recip $ negate (fromInteger 2 / sqrt pi) * exp (negate x * x)
        invnormcdf = lift1 invnormcdf $ \x -> recip $ (fromInteger (-1) / sqrt pi) * exp (x * x * fromRational (- recip 2) / sqrt (fromInteger 2))
      |]
