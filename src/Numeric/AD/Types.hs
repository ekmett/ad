{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2010-2014
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------
module Numeric.AD.Types
  (
  -- * AD modes
    Mode(..)
  -- * Jets
  , Jet(..)
  , headJet
  , tailJet
  , jet
  ) where

import Numeric.AD.Internal.Jet
import Numeric.AD.Internal.Classes
