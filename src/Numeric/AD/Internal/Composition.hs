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
import Numeric.AD.Mode

#ifdef HLINT
{-# ANN module "Hlint: ignore Eta reduce" #-}
{-# ANN module "Hlint: ignore Reduce duplication" #-}
#endif

------------------------------------------------------------------------------
-- ComposeMode
------------------------------------------------------------------------------

-- | The composition of two AD modes is an AD mode in its own right
newtype ComposeMode t = ComposeMode { decomposeMode :: t } deriving
  ( Eq, Enum, Ord, Bounded
  , Num, Real, Fractional
  , RealFrac, Floating, Erf
  , InvErf, RealFloat, Typeable
  )

type instance Scalar (ComposeMode t) = Scalar (Scalar t)

instance (Mode t, Mode (Scalar t)) => Mode (ComposeMode t) where
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
