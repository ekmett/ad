{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, FlexibleContexts, FlexibleInstances #-}
module Numeric.AD.Internal
    ( AD(..)
    , Id(..)
    ) where

import Control.Applicative
import Language.Haskell.TH
import Numeric.AD.Classes
import Data.Monoid

-- | Lift all the higher-kinded numeric classes to their traditional first-order equivalents
-- This lets us use (forall s. Mode s => AD s a) as any appropriate numerical type
-- concealing the automatic differentiation mode being used and limiting ourselves to safe operations.
newtype AD f a = AD { runAD :: f a } deriving (Lifted, Mode, Primal)

let f = varT (mkName "f") in deriveNumeric (conT ''AD `appT` f) f

newtype Id a = Id a deriving
    (Eq, Ord, Show, Enum, Bounded, Num, Real, Fractional, Floating, RealFrac, RealFloat, Monoid)

instance Functor Id where
    fmap f (Id a) = Id (f a)

instance Applicative Id where
    pure = Id
    Id f <*> Id a = Id (f a)

instance Monad Id where
    return = Id
    Id a >>= f = f a 

instance Lifted Id where
    (==!) = (==)
    compare1 = compare
    showsPrec1 = showsPrec
    fromInteger1 = fromInteger
    (+!) = (+)
    (-!) = (-)
    (*!) = (*) 
    negate1 = negate
    abs1 = abs
    signum1 = signum
    (/!) = (/)
    recip1 = recip
    fromRational1 = fromRational
    toRational1 = toRational   
    pi1 = pi
    exp1 = exp
    log1 = log
    sqrt1 = sqrt
    (**!) = (**)
    logBase1 = logBase
    sin1 = sin
    cos1 = cos
    tan1 = tan
    asin1 = asin
    acos1 = acos
    atan1 = atan
    sinh1 = sinh
    cosh1 = cosh
    tanh1 = tanh
    asinh1 = asinh
    acosh1 = acosh
    atanh1 = atanh
    properFraction1 = properFraction
    truncate1 = truncate
    round1 = round
    ceiling1 = ceiling
    floor1 = floor
    floatRadix1 = floatRadix
    floatDigits1 = floatDigits
    floatRange1 = floatRange
    decodeFloat1 = decodeFloat
    encodeFloat1 = encodeFloat
    exponent1 = exponent
    significand1 = significand
    scaleFloat1 = scaleFloat
    isNaN1 = isNaN
    isInfinite1 = isInfinite
    isDenormalized1 = isDenormalized
    isNegativeZero1 = isNegativeZero
    isIEEE1 = isIEEE
    atan21 = atan2
    succ1 = succ
    pred1 = pred
    toEnum1 = toEnum
    fromEnum1 = fromEnum
    enumFrom1 = enumFrom
    enumFromThen1 = enumFromThen
    enumFromTo1 = enumFromTo
    enumFromThenTo1 = enumFromThenTo
    minBound1 = minBound
    maxBound1 = maxBound

instance Mode Id where
    lift = Id
    Id a ^* b = Id (a * b)
    a *^ Id b = Id (a * b)
    Id a <+> Id b = Id (a + b)

instance Primal Id where
    primal (Id a) = a

-- not Jacobian -- its kind of pointless
