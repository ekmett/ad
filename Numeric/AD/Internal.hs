{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Internal
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------
module Numeric.AD.Internal
    ( zipWithT
    , zipWithDefaultT
    , on
    , AD(..)
    , Id(..)
    , probe
    , unprobe
    , probed
    , unprobed
    , Pair(..)
    ) where

import Control.Applicative
import Language.Haskell.TH
import Numeric.AD.Classes
import Data.Monoid
import Data.Traversable (Traversable, mapAccumL)
import Data.Foldable (Foldable, toList)

on :: (a -> a -> b) -> (c -> a) -> c -> c -> b
on f g a b = f (g a) (g b)

data Pair a b = Pair a b deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

zipWithT :: (Foldable f, Traversable g) => (a -> b -> c) -> f a -> g b -> g c
zipWithT f as = snd . mapAccumL (\(a:as') b -> (as', f a b)) (toList as)

zipWithDefaultT :: (Foldable f, Traversable g) => a -> (a -> b -> c) -> f a -> g b -> g c
zipWithDefaultT z f as = zipWithT f (toList as ++ repeat z)

class Iso a b where
    iso :: f a -> f b
    osi :: f b -> f a

instance Iso a a where
    iso = id
    osi = id

-- | 'AD' serves as a common wrapper for different 'Mode' instances, exposing a traditional
-- numerical tower. Universal quantification is used to limit the actions in user code to
-- machinery that will return the same answers under all AD modes, allowing us to use modes
-- interchangeably as both the type level \"brand\" and dictionary, providing a common API.
newtype AD f a = AD { runAD :: f a } deriving (Iso (f a), Lifted, Mode, Primal)


-- > instance (Lifted f, Num a) => Num (AD f a)
-- etc.
let f = varT (mkName "f") in 
    deriveNumeric 
        (classP ''Lifted [f]:) 
        (conT ''AD `appT` f)

newtype Id a = Id a deriving
    (Iso a, Eq, Ord, Show, Enum, Bounded, Num, Real, Fractional, Floating, RealFrac, RealFloat, Monoid)

probe :: a -> AD Id a
probe a = AD (Id a)

unprobe :: AD Id a -> a
unprobe (AD (Id a)) = a

pid :: f a -> f (Id a)
pid = iso

unpid :: f (Id a) -> f a
unpid = osi

probed :: f a -> f (AD Id a)
probed = iso . pid

unprobed :: f (AD Id a) -> f a
unprobed = unpid . osi

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
