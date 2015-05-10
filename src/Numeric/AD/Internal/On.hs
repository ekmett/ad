{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2010-2015
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------

module Numeric.AD.Internal.On
  ( On(..)
  ) where

import Data.Number.Erf
import Data.Data
import Numeric.AD.Mode

#ifdef HLINT
#endif

------------------------------------------------------------------------------
-- On
------------------------------------------------------------------------------

-- | The composition of two AD modes is an AD mode in its own right
newtype On t = On { off :: t } deriving
  ( Eq, Enum, Ord, Bounded
  , Num, Real, Fractional
  , RealFrac, Floating, Erf
  , InvErf, RealFloat, Typeable
  )

instance (Mode t, Mode (Scalar t)) => Mode (On t) where
  type Scalar (On t) = Scalar (Scalar t)
  auto = On . auto . auto
  a *^ On b = On (auto a *^ b)
  On a ^* b = On (a ^* auto b)
