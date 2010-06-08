{-# LANGUAGE Rank2Types, GeneralizedNewtypeDeriving, TemplateHaskell, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
-- {-# OPTIONS_HADDOCK hide, prune #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Internal.Types
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------
module Numeric.AD.Internal.Types
    ( AD(..)
    , UU, UF, FU, FF
    ) where

import Data.Data (Data(..), mkDataType, DataType, mkConstr, Constr, constrIndex, Fixity(..))
import Data.Typeable (Typeable1(..), Typeable(..), TyCon, mkTyCon, mkTyConApp, typeOfDefault, gcast1)
import Language.Haskell.TH
import Numeric.AD.Internal.Classes

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

-- | A scalar-to-scalar automatically-differentiable function.
type UU a = forall s. Mode s => AD s a -> AD s a
-- | A scalar-to-non-scalar automatically-differentiable function.
type UF f a = forall s. Mode s => AD s a -> f (AD s a)
-- | A non-scalar-to-scalar automatically-differentiable function.
type FU f a = forall s. Mode s => f (AD s a) -> AD s a
-- | A non-scalar-to-non-scalar automatically-differentiable function.
type FF f g a = forall s. Mode s => f (AD s a) -> g (AD s a)

instance Typeable1 f => Typeable1 (AD f) where
    typeOf1 tfa = mkTyConApp adTyCon [typeOf1 (undefined `asArgsType` tfa)]
        where asArgsType :: f a -> t f a -> f a
              asArgsType = const

instance (Typeable1 f, Typeable a) => Typeable (AD f a) where
    typeOf = typeOfDefault
    
adTyCon :: TyCon
adTyCon = mkTyCon "Numeric.AD.Internal.Types.AD"
{-# NOINLINE adTyCon #-}

adConstr :: Constr
adConstr = mkConstr adDataType "AD" [] Prefix
{-# NOINLINE adConstr #-}

adDataType :: DataType
adDataType = mkDataType "Numeric.AD.Internal.Types.AD" [adConstr]
{-# NOINLINE adDataType #-}

instance (Typeable1 f, Typeable a, Data (f a), Data a) => Data (AD f a) where
    gfoldl f z (AD a) = z AD `f` a
    toConstr _ = adConstr
    gunfold k z c = case constrIndex c of
        1 -> k (z AD)
        _ -> error "gunfold"
    dataTypeOf _ = adDataType
    dataCast1 f = gcast1 f
