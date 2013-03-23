{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, Rank2Types, StandaloneDeriving, TypeFamilies, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, TypeOperators #-}
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
import Data.Number.Erf
#if MIN_VERSION_base(4,4,0)
import Data.Typeable (Typeable1(..), Typeable(..), TyCon, mkTyCon3, mkTyConApp, typeOfDefault, gcast1)
#else
import Data.Typeable (Typeable1(..), Typeable(..), TyCon, mkTyCon, mkTyConApp, typeOfDefault, gcast1)
#endif
import Data.Foldable (Foldable(foldMap))
import Data.Traversable (Traversable(traverse))
import Numeric.AD.Internal.Classes
import Numeric.AD.Internal.Types

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
newtype ComposeMode f g s a = ComposeMode { runComposeMode :: f (AD s (g a)) }

deriving instance Enum (f (AD s (g a))) => Enum (ComposeMode f g s a)
deriving instance Eq (f (AD s (g a))) => Eq (ComposeMode f g s a)
deriving instance Ord (f (AD s (g a))) => Ord (ComposeMode f g s a)
deriving instance Bounded (f (AD s (g a))) => Bounded (ComposeMode f g s a)
deriving instance Num (f (AD s (g a))) => Num (ComposeMode f g s a)
deriving instance Fractional (f (AD s (g a))) => Fractional (ComposeMode f g s a)
deriving instance Floating (f (AD s (g a))) => Floating (ComposeMode f g s a)
deriving instance RealFloat (f (AD s (g a))) => RealFloat (ComposeMode f g s a)
deriving instance RealFrac (f (AD s (g a))) => RealFrac (ComposeMode f g s a)
deriving instance Real (f (AD s (g a))) => Real (ComposeMode f g s a)
deriving instance Erf (f (AD s (g a))) => Erf (ComposeMode f g s a)
deriving instance InvErf (f (AD s (g a))) => InvErf (ComposeMode f g s a)

type instance Scalar (ComposeMode f g s a) = a

composeMode :: AD s' (f (AD s (g a))) -> AD s' (ComposeMode f g s a)
composeMode (AD a) = AD (ComposeMode a)

decomposeMode :: AD s' (ComposeMode f g s a) -> AD s' (f (AD s (g a)))
decomposeMode (AD (ComposeMode a)) = AD a

instance (Primal (f (AD s (g a))), Mode (g a), Primal (g a), Scalar (f (AD s (g a))) ~ AD s (g a), Num (g a), Scalar (g a) ~ a) => Primal (ComposeMode f g s a) where
    primal = primal . primal . runComposeMode

instance (Mode (f (AD s (g a))), Mode (g a), Scalar (f (AD s (g a))) ~ AD s (g a), Scalar (g a) ~ a, Floating (g a)) => Mode (ComposeMode f g s a) where
    auto = ComposeMode . auto . auto
    ComposeMode a <+> ComposeMode b = ComposeMode (a <+> b)
    a *^ ComposeMode b = ComposeMode (auto a *^ b)
    ComposeMode a ^* b = ComposeMode (a ^* auto b)
    ComposeMode a ^/ b = ComposeMode (a ^/ auto b)
    ComposeMode a <**> ComposeMode b = ComposeMode (a <**> b)

instance (Typeable1 f, Typeable1 g) => Typeable1 (ComposeMode f g s) where
    typeOf1 tfga = mkTyConApp composeModeTyCon [typeOf1 (fa tfga), typeOf1 (ga tfga)]
        where fa :: t f (g :: * -> *) s a -> f a
              fa = undefined
              ga :: t (f :: * -> *) g s a -> g a
              ga = undefined

instance (Typeable1 f, Typeable1 g, Typeable a) => Typeable (ComposeMode f g s a) where
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

instance (Typeable1 f, Typeable1 g, Data (f (AD s (g a))), Data a) => Data (ComposeMode f g s a) where
    gfoldl f z (ComposeMode a) = z ComposeMode `f` a
    toConstr _ = composeModeConstr
    gunfold k z c = case constrIndex c of
        1 -> k (z ComposeMode)
        _ -> error "gunfold"
    dataTypeOf _ = composeModeDataType
    dataCast1 f = gcast1 f

