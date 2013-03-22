{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types, GeneralizedNewtypeDeriving, TemplateHaskell, TypeFamilies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK hide #-}
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
    ) where

#ifndef MIN_VERSION_base
#define MIN_VERSION_base (x,y,z) 1
#endif

import Data.Data (Data)
import Data.Number.Erf
import Data.Typeable (Typeable)
import Numeric.AD.Internal.Classes

{-# ANN module "HLint: ignore Eta reduce" #-}

-- | 'AD' serves as a common wrapper for different 'Mode' instances, exposing a traditional
-- numerical tower. Universal quantification is used to limit the actions in user code to
-- machinery that will return the same answers under all AD modes, allowing us to use modes
-- interchangeably as both the type level \"brand\" and dictionary, providing a common API.
newtype AD s a = AD { runAD :: a } deriving (Iso a, Mode, Primal, Typeable, Data, Enum, Eq, Ord, Bounded, Num, Fractional, Floating, RealFloat, RealFrac, Real, Erf, InvErf)

type instance Domain (AD s f) = Domain f
