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
  ( ComposeMode(..)
  ) where

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

import Data.Number.Erf
import Data.Data
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
