{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures, Rank2Types, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, PatternGuards, CPP #-}
{-# LANGUAGE FlexibleContexts, FunctionalDependencies, UndecidableInstances, GeneralizedNewtypeDeriving, TemplateHaskell #-}
-- {-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Internal.Classes
-- Copyright   :  (c) Edward Kmett 2010
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
    -- * Automatically Deriving AD
    , Jacobian(..)
    , Primal(..)
    , deriveNumeric
    , Iso(..)
    , Scalar
    ) where

import Control.Applicative ((<$>), pure)
import Control.Monad
import Data.Number.Erf
import Language.Haskell.TH.Syntax

type family Scalar t :: *

class Iso a b where
    iso :: f a -> f b
    osi :: f b -> f a

instance Iso a a where
    iso = id
    osi = id

infixr 6 <+>
infixr 7 *^
infixl 7 ^*
infixr 7 ^/
infixr 8 <**>

class Mode t s where
    -- | allowed to return False for items with a zero derivative, but we'll give more NaNs than strictly necessary
    isKnownConstant :: t s -> Bool
    isKnownConstant _ = False

    -- | allowed to return False for zero, but we give more NaN's than strictly necessary then
    isKnownZero :: Num (Scalar (t s)) => t s -> Bool
    isKnownZero _ = False

    -- | Embed a constant
    auto  :: Num (Scalar (t s)) => Scalar (t s) -> t s

    -- | Vector sum
    (<+>) :: Num (Scalar (t s)) => t s -> t s -> t s

    -- | Scalar-vector multiplication
    (*^) :: Num (Scalar (t s)) => Scalar (t s) -> t s -> t s

    -- | Vector-scalar multiplication
    (^*) :: Num (Scalar (t s)) => t s -> Scalar (t s) -> t s

    -- | Scalar division
    (^/) :: Fractional (Scalar (t s)) => t s -> Scalar (t s) -> t s

    -- | Exponentiation, this should be overloaded if you can figure out anything about what is constant!
    (<**>) :: Floating (Scalar (t s)) => t s -> t s -> t s

    -- default (<**>) :: (Jacobian t, Floating (D t), Floating (Scalar t)) => t -> t -> t
    -- x <**> y = lift2_ (**) (\z xi yi -> (yi * z / xi, z * log xi)) x y

    -- | > 'zero' = 'lift' 0
    zero :: Num (Scalar (t s)) => t s

#ifndef HLINT
    default (*^) :: (Num (t s), Num (Scalar (t s))) => Scalar (t s) -> t s -> t s
    a *^ b = auto a * b
    default (^*) :: (Num (t s), Num (Scalar (t s))) => t s -> Scalar (t s) -> t s
    a ^* b = a * auto b
#endif

    a ^/ b = a ^* recip b

    zero = auto 0

one :: (Mode t s, Num (Scalar (t s))) => t s
one = auto 1
{-# INLINE one #-}

negOne :: (Mode t s, Num (Scalar (t s))) => t s
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
    primal :: Num (Scalar t) => t -> Scalar t

-- | 'Jacobian' is used by 'deriveMode' but is not exposed
-- via 'Mode' to prevent its abuse by end users
-- via the 'AD' data type.
class (Mode t s, Mode (D t) s) => Jacobian t s where
    type D t :: * -> *

    unary  :: (Num a, a ~ Scalar (t s)) => (a -> a) -> D t s -> t s -> t s
    lift1  :: (Num a, a ~ Scalar (t s)) => (a -> a) -> (D t s -> D t s) -> t s -> t s
    lift1_ :: (Num a, a ~ Scalar (t s)) => (a -> a) -> (D t s -> D t s -> D t s) -> t s -> t s

    binary :: (Num a, a ~ Scalar (t s)) => (a -> a -> a) -> D t s -> D t s -> t s -> t s -> t s
    lift2  :: (Num a, a ~ Scalar (t s)) => (a -> a -> a) -> (D t s -> D t s -> (D t s, D t s)) -> t s -> t s -> t s
    lift2_ :: (Num a, a ~ Scalar (t s)) => (a -> a -> a) -> (D t s -> D t s -> D t s -> (D t s, D t s)) -> t s -> t s -> t s

withPrimal :: (Jacobian t s, Num a, a ~ Scalar (t s), Scalar (t s) ~ Scalar (D t s)) => t s -> a -> t s
withPrimal t a = unary (const a) one t
{-# INLINE withPrimal #-}

fromBy :: (Jacobian t s, Num a, Num (D t s), a ~ Scalar (t s), Scalar (t s) ~ Scalar (D t s)) => t s -> t s -> Int -> a -> t s
fromBy a delta n x = binary (\_ _ -> x) one (fromIntegral n) a delta

discrete1 :: (Primal t, Num a, a ~ Scalar t) => (a -> c) -> t -> c
discrete1 f x = f (primal x)
{-# INLINE discrete1 #-}

discrete2 :: (Primal t, Num a, a ~ Scalar t) => (a -> a -> c) -> t -> t -> c
discrete2 f x y = f (primal x) (primal y)
{-# INLINE discrete2 #-}

discrete3 :: (Primal t, Num a, a ~ Scalar t) => (a -> a -> a -> d) -> t -> t -> t -> d
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
       instance (Num a, Eq a) => Eq ($t a $s) where
        (==)          = discrete2 (==)
       instance (Num a, Ord a) => Ord ($t a $s) where
        compare       = discrete2 compare
       instance (Num a, Bounded a) => Bounded ($t a $s) where
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
