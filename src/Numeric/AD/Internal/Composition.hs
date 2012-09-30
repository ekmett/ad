{-# LANGUAGE CPP #-}
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
-----------------------------------------------------------------------------

module Numeric.AD.Internal.Composition
    ( ComposeFunctor(..)
    , ComposeMode(..)
    , composeMode
    , decomposeMode
    ) where

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

import Control.Applicative hiding ((<**>))
import Data.Data (Data(..), mkDataType, DataType, mkConstr, Constr, constrIndex, Fixity(..))
#if MIN_VERSION_base(4,4,0)
import Data.Typeable (Typeable1(..), Typeable(..), TyCon, mkTyCon3, mkTyConApp, typeOfDefault, gcast1)
#else
import Data.Typeable (Typeable1(..), Typeable(..), TyCon, mkTyCon, mkTyConApp, typeOfDefault, gcast1)
#endif
import Data.Foldable (Foldable(foldMap))
import Data.Traversable (Traversable(traverse))
import Numeric.AD.Internal.Classes
import Numeric.AD.Internal.Types

-- | Functor composition, used to nest the use of jacobian and grad
newtype ComposeFunctor f g a = ComposeFunctor { decomposeFunctor :: f (g a) }

instance (Functor f, Functor g) => Functor (ComposeFunctor f g) where
    fmap f (ComposeFunctor a) = ComposeFunctor (fmap (fmap f) a)

instance (Foldable f, Foldable g) => Foldable (ComposeFunctor f g) where
    foldMap f (ComposeFunctor a) = foldMap (foldMap f) a

instance (Traversable f, Traversable g) => Traversable (ComposeFunctor f g) where
    traverse f (ComposeFunctor a) = ComposeFunctor <$> traverse (traverse f) a

instance (Typeable1 f, Typeable1 g) => Typeable1 (ComposeFunctor f g) where
    typeOf1 tfga = mkTyConApp composeFunctorTyCon [typeOf1 (fa tfga), typeOf1 (ga tfga)]
        where fa :: t f (g :: * -> *) a -> f a
              fa = undefined
              ga :: t (f :: * -> *) g a -> g a
              ga = undefined

composeFunctorTyCon :: TyCon
#if MIN_VERSION_base(4,4,0)
composeFunctorTyCon = mkTyCon3 "ad" "Numeric.AD.Internal.Composition" "ComposeFunctor"
#else
composeFunctorTyCon = mkTyCon "Numeric.AD.Internal.Composition.ComposeFunctor"
#endif

{-# NOINLINE composeFunctorTyCon #-}

composeFunctorConstr :: Constr
composeFunctorConstr = mkConstr composeFunctorDataType "ComposeFunctor" [] Prefix
{-# NOINLINE composeFunctorConstr #-}

composeFunctorDataType :: DataType
composeFunctorDataType = mkDataType "Numeric.AD.Internal.Composition.ComposeFunctor" [composeFunctorConstr]
{-# NOINLINE composeFunctorDataType #-}

instance (Typeable1 f, Typeable1 g, Data (f (g a)), Data a) => Data (ComposeFunctor f g a) where
    gfoldl f z (ComposeFunctor a) = z ComposeFunctor `f` a
    toConstr _ = composeFunctorConstr
    gunfold k z c = case constrIndex c of
        1 -> k (z ComposeFunctor)
        _ -> error "gunfold"
    dataTypeOf _ = composeFunctorDataType
    dataCast1 f = gcast1 f

-- | The composition of two AD modes is an AD mode in its own right
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
    ComposeMode a <**> ComposeMode b = ComposeMode (a <**> b)

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

instance (Typeable1 f, Typeable1 g) => Typeable1 (ComposeMode f g) where
    typeOf1 tfga = mkTyConApp composeModeTyCon [typeOf1 (fa tfga), typeOf1 (ga tfga)]
        where fa :: t f (g :: * -> *) a -> f a
              fa = undefined
              ga :: t (f :: * -> *) g a -> g a
              ga = undefined

instance (Typeable1 f, Typeable1 g, Typeable a) => Typeable (ComposeMode f g a) where
    typeOf = typeOfDefault

composeModeTyCon :: TyCon
#if MIN_VERSION_base(4,4,0)
composeModeTyCon = mkTyCon3 "ad" "Numeric.AD.Internal.Composition" "ComposeMode"
#else
composeModeTyCon = mkTyCon "Numeric.AD.Internal.Composition.ComposeMode"
#endif
{-# NOINLINE composeModeTyCon #-}

composeModeConstr :: Constr
composeModeConstr = mkConstr composeModeDataType "ComposeMode" [] Prefix
{-# NOINLINE composeModeConstr #-}

composeModeDataType :: DataType
composeModeDataType = mkDataType "Numeric.AD.Internal.Composition.ComposeMode" [composeModeConstr]
{-# NOINLINE composeModeDataType #-}

instance (Typeable1 f, Typeable1 g, Data (f (AD g a)), Data a) => Data (ComposeMode f g a) where
    gfoldl f z (ComposeMode a) = z ComposeMode `f` a
    toConstr _ = composeModeConstr
    gunfold k z c = case constrIndex c of
        1 -> k (z ComposeMode)
        _ -> error "gunfold"
    dataTypeOf _ = composeModeDataType
    dataCast1 f = gcast1 f

