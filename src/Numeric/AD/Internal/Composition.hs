{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, Rank2Types, StandaloneDeriving, TypeFamilies, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, TypeOperators #-}
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
#endif
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
import Data.Number.Erf
import Data.Data
import Data.Foldable (Foldable(foldMap))
import Data.Traversable (Traversable(traverse))
import Numeric.AD.Internal.Classes

{-# ANN module "Hlint: ignore Eta reduce" #-}
{-# ANN module "Hlint: ignore Reduce duplication" #-}

-- | Functor composition, used to nest the use of jacobian and grad
newtype ComposeFunctor f g a = ComposeFunctor { decomposeFunctor :: f (g a) }

instance (Functor f, Functor g) => Functor (ComposeFunctor f g) where
    fmap f (ComposeFunctor a) = ComposeFunctor (fmap (fmap f) a)

instance (Foldable f, Foldable g) => Foldable (ComposeFunctor f g) where
    foldMap f (ComposeFunctor a) = foldMap (foldMap f) a

instance (Traversable f, Traversable g) => Traversable (ComposeFunctor f g) where
    traverse f (ComposeFunctor a) = ComposeFunctor <$> traverse (traverse f) a

#if __GLASGOW_HASKELL__ >= 707
deriving instance Typeable ComposeFunctor
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

-- | The composition of two AD modes is an AD mode in its own right
newtype ComposeMode f g a s = ComposeMode { runComposeMode :: f (g a s) s}

deriving instance Enum (f (g a s) s) => Enum (ComposeMode f g a s)
deriving instance Eq (f (g a s) s) => Eq (ComposeMode f g a s)
deriving instance Ord (f (g a s) s) => Ord (ComposeMode f g a s)
deriving instance Bounded (f (g a s) s) => Bounded (ComposeMode f g a s)
deriving instance Num (f (g a s) s) => Num (ComposeMode f g a s)
deriving instance Fractional (f (g a s) s) => Fractional (ComposeMode f g a s)
deriving instance Floating (f (g a s) s) => Floating (ComposeMode f g a s)
deriving instance RealFloat (f (g a s) s) => RealFloat (ComposeMode f g a s)
deriving instance RealFrac (f (g a s) s) => RealFrac (ComposeMode f g a s)
deriving instance Real (f (g a s) s) => Real (ComposeMode f g a s)
deriving instance Erf (f (g a s) s) => Erf (ComposeMode f g a s)
deriving instance InvErf (f (g a s) s) => InvErf (ComposeMode f g a s)

type instance Scalar (ComposeMode f g a s s') = a

composeMode :: (f (g a s) s') -> ComposeMode f g a s s'
composeMode = ComposeMode

decomposeMode :: ComposeMode f g a s s' -> f (g a s) s'
decomposeMode = runComposeMode

instance (Primal (f (g a s) s'), Mode (g a) s, Primal (g a s), Scalar (f (g a s) s') ~ g a s, Num (g a s), Scalar (g a s) ~ a) => Primal (ComposeMode f g a s s') where
    primal = primal . primal . runComposeMode

instance (Mode (f (g a s)) s', Mode (g a) s, Scalar (f (g a s) s') ~ g a s, Scalar (g a s) ~ a, Floating (g a s)) => Mode (ComposeMode f g a s) s' where
    auto = ComposeMode . auto . auto
    ComposeMode a <+> ComposeMode b = ComposeMode (a <+> b)
    a *^ ComposeMode b = ComposeMode (auto a *^ b)
    ComposeMode a ^* b = ComposeMode (a ^* auto b)
    ComposeMode a ^/ b = ComposeMode (a ^/ auto b)
    ComposeMode a <**> ComposeMode b = ComposeMode (a <**> b)

#if __GLASGOW_HASKELL__ >= 707
deriving instance Typeable ComposeMode
deriving instance (Typeable f, Typeable g, Typeable s, Data (f (g a s) s'), Data a) => Data (ComposeMode f g a s s')
#else
instance (Typeable2 f, Typeable2 g) => Typeable3 (ComposeMode f g) where
    typeOf3 tfg = mkTyConApp composeModeTyCon [typeOf2 (fa tfg), typeOf2 (ga tfg)]
        where fa :: t f (g :: * -> * -> *) a s s' -> f a s'
              fa = undefined
              ga :: t (f :: * -> * -> *) g a s s'-> g a s
              ga = undefined

instance (Typeable2 f, Typeable2 g, Typeable a, Typeable s, Typeable s') => Typeable (ComposeMode f g a s s') where
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

instance (Typeable2 f, Typeable2 g, Data (f (g a s) s'), Data a, Typeable s', Typeable s, Data s') => Data (ComposeMode f g a s s') where
    gfoldl f z (ComposeMode a) = z ComposeMode `f` a
    toConstr _ = composeModeConstr
    gunfold k z c = case constrIndex c of
        1 -> k (z ComposeMode)
        _ -> error "gunfold"
    dataTypeOf _ = composeModeDataType
    dataCast1 f = gcast1 f
#endif

