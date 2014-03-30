{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

type instance Scalar (On t) = Scalar (Scalar t)

instance (Mode t, Mode (Scalar t)) => Mode (On t) where
  auto = On . auto . auto
  On a <+> On b = On (a <+> b)
  a *^ On b = On (auto a *^ b)
  On a ^* b = On (a ^* auto b)
