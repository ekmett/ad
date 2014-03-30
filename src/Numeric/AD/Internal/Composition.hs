{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2010-2014
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------

module Numeric.AD.Internal.Composition
  ( ComposeFunctor(..)
  , ComposeMode(..)
  ) where

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

import Control.Applicative hiding ((<**>))
import Data.Number.Erf
import Data.Data
import Data.Foldable (Foldable(foldMap))
#if __GLASGOW_HASKELL__ < 707
import Data.Proxy
#endif
import Data.Traversable (Traversable(traverse))
import Numeric.AD.Internal.Classes

#ifdef HLINT
{-# ANN module "Hlint: ignore Eta reduce" #-}
{-# ANN module "Hlint: ignore Reduce duplication" #-}
#endif

------------------------------------------------------------------------------
-- ComposeMode
------------------------------------------------------------------------------

-- | The composition of two AD modes is an AD mode in its own right
newtype ComposeMode f a s = ComposeMode { decomposeMode :: f a s }
#if __GLASGOW_HASKELL__ >= 707
  deriving Typeable
#endif

type instance Scalar (ComposeMode f a s) = Scalar a

instance (Lifted (f a s), Eq a, Num a, Scalar (f a s) ~ a) => Eq (ComposeMode f a s) where
  ComposeMode a == ComposeMode b = liftEq [a] $ a == b

instance (Lifted (f a s), Enum a, Num a, Scalar (f a s) ~ a) => Enum (ComposeMode f a s) where
  succ (ComposeMode a) = ComposeMode $ liftedEnum (succ a)
  pred (ComposeMode a) = ComposeMode $ liftedEnum (pred a)
  fromEnum (ComposeMode a) = liftEnum [a] (fromEnum a)
  toEnum i = ComposeMode $ liftedEnum $ toEnum i
  enumFrom (ComposeMode a) = ComposeMode <$> liftEnum [a] (enumFrom a)
  enumFromTo (ComposeMode a) (ComposeMode b) = ComposeMode <$> liftEnum [a] (enumFromTo a b)
  enumFromThen (ComposeMode a) (ComposeMode b) = ComposeMode <$> liftEnum [a] (enumFromThen a b)
  enumFromThenTo (ComposeMode a) (ComposeMode b) (ComposeMode c) = ComposeMode <$> liftEnum [a] (enumFromThenTo a b c)

instance (Lifted (f a s), Ord a, Num a, Scalar (f a s) ~ a) => Ord (ComposeMode f a s) where
  compare (ComposeMode a) (ComposeMode b) = liftOrd [a] $ compare a b

instance (Lifted (f a s), Bounded a, Num a, Scalar (f a s) ~ a) => Bounded (ComposeMode f a s) where
  minBound = ComposeMode $ liftedBounded minBound
  maxBound = ComposeMode $ liftedBounded minBound

instance (Lifted (f a s), Num a, Scalar (f a s) ~ a) => Num (ComposeMode f a s) where
  ComposeMode a + ComposeMode b = ComposeMode $ liftedNum (a + b)
  ComposeMode a - ComposeMode b = ComposeMode $ liftedNum (a - b)
  ComposeMode a * ComposeMode b = ComposeMode $ liftedNum (a * b)
  abs (ComposeMode a) = ComposeMode $ liftedNum (abs a)
  signum (ComposeMode a) = ComposeMode $ liftedNum (signum a)
  fromInteger i = ComposeMode $ liftedNum $ fromInteger i

instance (Lifted (f a s), Real a, Scalar (f a s) ~ a) => Real (ComposeMode f a s) where
  toRational (ComposeMode a) = liftReal [a] $ toRational a

instance (Lifted (f a s), Fractional a, Scalar (f a s) ~ a) => Fractional (ComposeMode f a s) where
  ComposeMode a / ComposeMode b = ComposeMode $ liftedFractional (a / b)
  recip (ComposeMode a) = ComposeMode $ liftedFractional (recip a)
  fromRational r = ComposeMode $ liftedFractional $ fromRational r

instance (Lifted (f a s), RealFrac a, Scalar (f a s) ~ a) => RealFrac (ComposeMode f a s) where
  properFraction (ComposeMode a) = liftRealFrac [a] $ case properFraction a of
    (i, b) -> (i, ComposeMode b)
  truncate (ComposeMode a) = liftRealFrac [a] (truncate a)
  round    (ComposeMode a) = liftRealFrac [a] (round a)
  ceiling  (ComposeMode a) = liftRealFrac [a] (ceiling a)
  floor    (ComposeMode a) = liftRealFrac [a] (floor a)

instance (Lifted (f a s), Floating a, Scalar (f a s) ~ a) => Floating (ComposeMode f a s) where
  pi = ComposeMode $ liftedFloating pi
  exp (ComposeMode a) = ComposeMode $ liftedFloating (exp a)
  sqrt (ComposeMode a) = ComposeMode $ liftedFloating (sqrt a)
  log (ComposeMode a) = ComposeMode $ liftedFloating (log a)
  ComposeMode a ** ComposeMode b = ComposeMode $ liftedFloating (a ** b)
  logBase (ComposeMode a) (ComposeMode b) = ComposeMode $ liftedFloating (logBase a b)
  sin (ComposeMode a) = ComposeMode $ liftedFloating (sin a)
  tan (ComposeMode a) = ComposeMode $ liftedFloating (tan a)
  cos (ComposeMode a) = ComposeMode $ liftedFloating (cos a)
  asin (ComposeMode a) = ComposeMode $ liftedFloating (asin a)
  atan (ComposeMode a) = ComposeMode $ liftedFloating (atan a)
  acos (ComposeMode a) = ComposeMode $ liftedFloating (acos a)
  sinh (ComposeMode a) = ComposeMode $ liftedFloating (sinh a)
  tanh (ComposeMode a) = ComposeMode $ liftedFloating (tanh a)
  cosh (ComposeMode a) = ComposeMode $ liftedFloating (cosh a)
  asinh (ComposeMode a) = ComposeMode $ liftedFloating (asinh a)
  atanh (ComposeMode a) = ComposeMode $ liftedFloating (atanh a)
  acosh (ComposeMode a) = ComposeMode $ liftedFloating (acosh a)

instance (Lifted (f a s), Erf a, Scalar (f a s) ~ a) => Erf (ComposeMode f a s) where
  erf (ComposeMode a) = ComposeMode $ liftedErf (erf a)
  erfc (ComposeMode a) = ComposeMode $ liftedErf (erfc a)

instance (Lifted (f a s), InvErf a, Scalar (f a s) ~ a) => InvErf (ComposeMode f a s) where
  inverf (ComposeMode a) = ComposeMode $ liftedInvErf (inverf a)
  inverfc (ComposeMode a) = ComposeMode $ liftedInvErf (inverfc a)
  invnormcdf (ComposeMode a) = ComposeMode $ liftedInvErf (invnormcdf a)

instance (Lifted (f a s), Primal a, Num a, Scalar (f a s) ~ a) => Primal (ComposeMode f a s) where
  primal (ComposeMode a) = liftPrimal [a] $ primal $ primal a

instance (Lifted (f a s), RealFloat a, Scalar (f a s) ~ a) => RealFloat (ComposeMode f a s) where
  floatRadix (ComposeMode a) = liftRealFloat [a] (floatRadix a)
  floatDigits (ComposeMode a) = liftRealFloat [a] (floatDigits a)
  floatRange (ComposeMode a) = liftRealFloat [a] (floatRange a)
  decodeFloat (ComposeMode a) = liftRealFloat [a] (decodeFloat a)
  encodeFloat i j = ComposeMode $ liftedRealFloat (encodeFloat i j)
  exponent (ComposeMode a) = liftRealFloat [a] (exponent a)
  significand (ComposeMode a) = ComposeMode $ liftedRealFloat (significand a)
  scaleFloat n (ComposeMode a) = ComposeMode $ liftedRealFloat (scaleFloat n a)
  isNaN (ComposeMode a) = liftRealFloat [a] (isNaN a)
  isInfinite (ComposeMode a) = liftRealFloat [a] (isInfinite a)
  isDenormalized (ComposeMode a) = liftRealFloat [a] (isDenormalized a)
  isNegativeZero (ComposeMode a) = liftRealFloat [a] (isNegativeZero a)
  isIEEE (ComposeMode a) = liftRealFloat [a] (isIEEE a)
  atan2 (ComposeMode a) (ComposeMode b) = ComposeMode $ liftedRealFloat (atan2 a b)

liftedFloating' :: (a ~ Scalar g, Floating a, Lifted g) =>
    (Floating g => h g s) -> h g s
liftedFloating' = liftFloating (Proxy :: Proxy g)

instance (Lifted (f a s), Lifted a, Mode a, Num a, Scalar (f a s) ~ a, Num (Scalar a)) => Mode (ComposeMode f a s) where
  auto a = ComposeMode $ liftedMode $ auto (auto a)
  ComposeMode a <+> ComposeMode b = ComposeMode $ liftedMode (a <+> b)
  a *^ ComposeMode b = ComposeMode $ liftedMode (auto a *^ b)
  ComposeMode a ^* b = ComposeMode $ liftedMode (a ^* auto b)
  -- ComposeMode a ^/ b = ComposeMode $ liftedMode (a ^/ auto b)

------------------------------------------------------------------------------
-- ComposeFunctor
------------------------------------------------------------------------------

-- | Functor composition, used to nest the use of jacobian and grad
newtype ComposeFunctor f g a = ComposeFunctor { decomposeFunctor :: f (g a) }
#if __GLASGOW_HASKELL__ >= 707
  deriving Typeable
#endif

instance (Functor f, Functor g) => Functor (ComposeFunctor f g) where
    fmap f (ComposeFunctor a) = ComposeFunctor (fmap (fmap f) a)

instance (Foldable f, Foldable g) => Foldable (ComposeFunctor f g) where
    foldMap f (ComposeFunctor a) = foldMap (foldMap f) a

instance (Traversable f, Traversable g) => Traversable (ComposeFunctor f g) where
    traverse f (ComposeFunctor a) = ComposeFunctor <$> traverse (traverse f) a

#if __GLASGOW_HASKELL__ >= 707
deriving instance (Typeable f, Typeable a, Typeable g, Data (f (g a))) => Data (ComposeFunctor f g a)
#else
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
#endif

