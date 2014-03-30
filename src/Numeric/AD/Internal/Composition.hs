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

import Control.Applicative
import Data.Number.Erf
import Data.Data
import Data.Foldable (Foldable(foldMap))
import Data.Traversable (Traversable(traverse))
import Numeric.AD.Internal.Jacobian
import Numeric.AD.Mode

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

instance (Eq (f a s)) => Eq (ComposeMode f a s) where
  ComposeMode a == ComposeMode b = a == b

instance (Enum (f a s)) => Enum (ComposeMode f a s) where
  succ (ComposeMode a) = ComposeMode (succ a)
  pred (ComposeMode a) = ComposeMode (pred a)
  fromEnum (ComposeMode a) = fromEnum a
  toEnum i = ComposeMode $ toEnum i
  enumFrom (ComposeMode a) = ComposeMode <$> enumFrom a
  enumFromTo (ComposeMode a) (ComposeMode b) = ComposeMode <$> enumFromTo a b
  enumFromThen (ComposeMode a) (ComposeMode b) = ComposeMode <$> enumFromThen a b
  enumFromThenTo (ComposeMode a) (ComposeMode b) (ComposeMode c) = ComposeMode <$> enumFromThenTo a b c

instance (Ord (f a s)) => Ord (ComposeMode f a s) where
  compare (ComposeMode a) (ComposeMode b) = compare a b

instance (Bounded (f a s)) => Bounded (ComposeMode f a s) where
  minBound = ComposeMode minBound
  maxBound = ComposeMode minBound

instance (Num (f a s)) => Num (ComposeMode f a s) where
  ComposeMode a + ComposeMode b = ComposeMode (a + b)
  ComposeMode a - ComposeMode b = ComposeMode (a - b)
  ComposeMode a * ComposeMode b = ComposeMode (a * b)
  abs (ComposeMode a) = ComposeMode (abs a)
  signum (ComposeMode a) = ComposeMode (signum a)
  fromInteger i = ComposeMode (fromInteger i)

instance (Real (f a s)) => Real (ComposeMode f a s) where
  toRational (ComposeMode a) = toRational a

instance (Fractional (f a s)) => Fractional (ComposeMode f a s) where
  ComposeMode a / ComposeMode b = ComposeMode (a / b)
  recip (ComposeMode a) = ComposeMode (recip a)
  fromRational r = ComposeMode (fromRational r)

instance (RealFrac (f a s)) => RealFrac (ComposeMode f a s) where
  properFraction (ComposeMode a) = case properFraction a of
    (i, b) -> (i, ComposeMode b)
  truncate (ComposeMode a) = truncate a
  round    (ComposeMode a) = round a
  ceiling  (ComposeMode a) = ceiling a
  floor    (ComposeMode a) = floor a

instance (Floating (f a s)) => Floating (ComposeMode f a s) where
  pi = ComposeMode pi
  exp (ComposeMode a) = ComposeMode (exp a)
  sqrt (ComposeMode a) = ComposeMode (sqrt a)
  log (ComposeMode a) = ComposeMode (log a)
  ComposeMode a ** ComposeMode b = ComposeMode (a ** b)
  logBase (ComposeMode a) (ComposeMode b) = ComposeMode (logBase a b)
  sin (ComposeMode a) = ComposeMode (sin a)
  tan (ComposeMode a) = ComposeMode (tan a)
  cos (ComposeMode a) = ComposeMode (cos a)
  asin (ComposeMode a) = ComposeMode (asin a)
  atan (ComposeMode a) = ComposeMode (atan a)
  acos (ComposeMode a) = ComposeMode (acos a)
  sinh (ComposeMode a) = ComposeMode (sinh a)
  tanh (ComposeMode a) = ComposeMode (tanh a)
  cosh (ComposeMode a) = ComposeMode (cosh a)
  asinh (ComposeMode a) = ComposeMode (asinh a)
  atanh (ComposeMode a) = ComposeMode (atanh a)
  acosh (ComposeMode a) = ComposeMode (acosh a)

instance (Erf (f a s)) => Erf (ComposeMode f a s) where
  erf (ComposeMode a) = ComposeMode (erf a)
  erfc (ComposeMode a) = ComposeMode (erfc a)

instance (InvErf (f a s)) => InvErf (ComposeMode f a s) where
  inverf (ComposeMode a) = ComposeMode (inverf a)
  inverfc (ComposeMode a) = ComposeMode (inverfc a)
  invnormcdf (ComposeMode a) = ComposeMode (invnormcdf a)

instance (RealFloat (f a s)) => RealFloat (ComposeMode f a s) where
  floatRadix (ComposeMode a) = floatRadix a
  floatDigits (ComposeMode a) = floatDigits a
  floatRange (ComposeMode a) = floatRange a
  decodeFloat (ComposeMode a) = decodeFloat a
  encodeFloat i j = ComposeMode (encodeFloat i j)
  exponent (ComposeMode a) = exponent a
  significand (ComposeMode a) = ComposeMode (significand a)
  scaleFloat n (ComposeMode a) = ComposeMode (scaleFloat n a)
  isNaN (ComposeMode a) = isNaN a
  isInfinite (ComposeMode a) =  isInfinite a
  isDenormalized (ComposeMode a) = isDenormalized a
  isNegativeZero (ComposeMode a) = isNegativeZero a
  isIEEE (ComposeMode a) = isIEEE a
  atan2 (ComposeMode a) (ComposeMode b) = ComposeMode $ atan2 a b

instance (Mode (f a s), Mode a, Scalar (f a s) ~ a) => Mode (ComposeMode f a s) where
  auto a = ComposeMode $ auto (auto a)
  ComposeMode a <+> ComposeMode b = ComposeMode (a <+> b)
  a *^ ComposeMode b = ComposeMode (auto a *^ b)
  ComposeMode a ^* b = ComposeMode (a ^* auto b)

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

