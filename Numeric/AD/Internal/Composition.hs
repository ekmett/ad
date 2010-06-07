{-# LANGUAGE Rank2Types, TypeFamilies, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TemplateHaskell, UndecidableInstances, TypeOperators #-}
-- {-# OPTIONS_HADDOCK hide, prune #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Internal.Composition
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Defines the composition of two AD modes as an AD mode in its own right
-----------------------------------------------------------------------------

module Numeric.AD.Internal.Composition
    ( ComposeFunctor(..)
    , ComposeMode(..)
    , composeMode
    , decomposeMode
    ) where

import Data.Traversable
import Control.Applicative
import Data.Foldable
import Numeric.AD.Internal

-- * Functor composition

newtype ComposeFunctor f g a = ComposeFunctor { decomposeFunctor :: f (g a) }

instance (Functor f, Functor g) => Functor (ComposeFunctor f g) where
    fmap f (ComposeFunctor a) = ComposeFunctor (fmap (fmap f) a)

instance (Foldable f, Foldable g) => Foldable (ComposeFunctor f g) where
    foldMap f (ComposeFunctor a) = foldMap (foldMap f) a

instance (Traversable f, Traversable g) => Traversable (ComposeFunctor f g) where
    traverse f (ComposeFunctor a) = ComposeFunctor <$> traverse (traverse f) a

newtype ComposeMode f g a = ComposeMode { runComposeMode :: f (AD g a) }

composeMode :: AD f (AD g a) -> AD (ComposeMode f g) a
composeMode (AD a) = AD (ComposeMode a)

decomposeMode :: AD (ComposeMode f g) a -> AD f (AD g a)
decomposeMode (AD (ComposeMode a)) = AD a

instance (Primal f, Mode g, Primal g) => Primal (ComposeMode f g) where
    primal = primal . primal . runComposeMode

instance (Mode f, Mode g) => Mode (ComposeMode f g) where
    lift = ComposeMode . lift . lift
    ComposeMode a <+> ComposeMode b = ComposeMode (a <+> b)
    a *^ ComposeMode b = ComposeMode (lift a *^ b)
    ComposeMode a ^* b = ComposeMode (a ^* lift b)
    ComposeMode a ^/ b = ComposeMode (a ^/ lift b)

instance (Mode f, Mode g) => Lifted (ComposeMode f g) where
    showsPrec1 n (ComposeMode a) = showsPrec1 n a
    ComposeMode a ==! ComposeMode b  = a ==! b
    compare1 (ComposeMode a) (ComposeMode b) = compare1 a b
    fromInteger1 = ComposeMode . lift . fromInteger1
    ComposeMode a +! ComposeMode b = ComposeMode (a +! b)
    ComposeMode a -! ComposeMode b = ComposeMode (a -! b)
    ComposeMode a *! ComposeMode b = ComposeMode (a *! b)
    negate1 (ComposeMode a) = ComposeMode (negate1 a)
    abs1 (ComposeMode a) = ComposeMode (abs1 a)
    signum1 (ComposeMode a) = ComposeMode (signum1 a)
    ComposeMode a /! ComposeMode b = ComposeMode (a /! b)
    recip1 (ComposeMode a) = ComposeMode (recip1 a)
    fromRational1 = ComposeMode . lift . fromRational1
    toRational1 (ComposeMode a) = toRational1 a
    pi1 = ComposeMode pi1
    exp1 (ComposeMode a) = ComposeMode (exp1 a)
    log1 (ComposeMode a) = ComposeMode (log1 a)
    sqrt1 (ComposeMode a) = ComposeMode (sqrt1 a)
    ComposeMode a **! ComposeMode b = ComposeMode (a **! b)
    logBase1 (ComposeMode a) (ComposeMode b) = ComposeMode (logBase1 a b)
    sin1 (ComposeMode a) = ComposeMode (sin1 a)
    cos1 (ComposeMode a) = ComposeMode (cos1 a)
    tan1 (ComposeMode a) = ComposeMode (tan1 a)
    asin1 (ComposeMode a) = ComposeMode (asin1 a)
    acos1 (ComposeMode a) = ComposeMode (acos1 a)
    atan1 (ComposeMode a) = ComposeMode (atan1 a)
    sinh1 (ComposeMode a) = ComposeMode (sinh1 a)
    cosh1 (ComposeMode a) = ComposeMode (cosh1 a)
    tanh1 (ComposeMode a) = ComposeMode (tanh1 a)
    asinh1 (ComposeMode a) = ComposeMode (asinh1 a)
    acosh1 (ComposeMode a) = ComposeMode (acosh1 a)
    atanh1 (ComposeMode a) = ComposeMode (atanh1 a)
    properFraction1 (ComposeMode a) = (b, ComposeMode c) where
        (b, c) = properFraction1 a
    truncate1 (ComposeMode a) = truncate1 a
    round1 (ComposeMode a) = round1 a
    ceiling1 (ComposeMode a) = ceiling1 a
    floor1 (ComposeMode a) = floor1 a
    floatRadix1 (ComposeMode a) = floatRadix1 a
    floatDigits1 (ComposeMode a) = floatDigits1 a
    floatRange1 (ComposeMode a) = floatRange1 a
    decodeFloat1 (ComposeMode a) = decodeFloat1 a
    encodeFloat1 m e = ComposeMode (encodeFloat1 m e)
    exponent1 (ComposeMode a) = exponent1 a
    significand1 (ComposeMode a) = ComposeMode (significand1 a)
    scaleFloat1 n (ComposeMode a) = ComposeMode (scaleFloat1 n a)
    isNaN1 (ComposeMode a) = isNaN1 a
    isInfinite1 (ComposeMode a) = isInfinite1 a
    isDenormalized1 (ComposeMode a) = isDenormalized1 a
    isNegativeZero1 (ComposeMode a) = isNegativeZero1 a
    isIEEE1 (ComposeMode a) = isIEEE1 a
    atan21 (ComposeMode a) (ComposeMode b) = ComposeMode (atan21 a b)
    succ1 (ComposeMode a) = ComposeMode (succ1 a)
    pred1 (ComposeMode a) = ComposeMode (pred1 a)
    toEnum1 n = ComposeMode (toEnum1 n)
    fromEnum1 (ComposeMode a) = fromEnum1 a
    enumFrom1 (ComposeMode a) = map ComposeMode $ enumFrom1 a
    enumFromThen1 (ComposeMode a) (ComposeMode b) = map ComposeMode $ enumFromThen1 a b
    enumFromTo1 (ComposeMode a) (ComposeMode b) = map ComposeMode $ enumFromTo1 a b
    enumFromThenTo1 (ComposeMode a) (ComposeMode b) (ComposeMode c) = map ComposeMode $ enumFromThenTo1 a b c
    minBound1 = ComposeMode minBound1
    maxBound1 = ComposeMode maxBound1

-- deriveNumeric (conT `appT` varT (mkName "f") `appT` varT (mkName "g"))
