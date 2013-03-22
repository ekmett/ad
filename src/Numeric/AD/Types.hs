{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Types
-- Copyright   :  (c) Edward Kmett 2010-12
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
    -- * AD variables
    , AD(..)
    -- * Jets
    , Jet(..)
    , headJet
    , tailJet
    , jet
    -- * Apply functions that use 'lift'
    , lowerUU, lowerUF, lowerFU, lowerFF
    ) where

import Numeric.AD.Internal.Identity
import Numeric.AD.Internal.Types
import Numeric.AD.Internal.Jet
import Numeric.AD.Internal.Classes

-- | Evaluate a scalar-to-scalar function in the trivial identity AD mode.
lowerUU :: (forall s. AD s (Id a) -> AD s (Id a)) -> a -> a
lowerUU f = unprobe . f . probe
{-# INLINE lowerUU #-}

-- | Evaluate a scalar-to-nonscalar function in the trivial identity AD mode.
lowerUF :: (forall s. AD s (Id a) -> f (AD s (Id a))) -> a -> f a
lowerUF f = unprobed . f . probe
{-# INLINE lowerUF #-}

-- | Evaluate a nonscalar-to-scalar function in the trivial identity AD mode.
lowerFU :: (forall s. f (AD s (Id a)) -> AD s (Id a)) -> f a -> a
lowerFU f = unprobe . f . probed
{-# INLINE lowerFU #-}

-- | Evaluate a nonscalar-to-nonscalar function in the trivial identity AD mode.
lowerFF :: (forall s. f (AD s (Id a)) -> g (AD s (Id a))) -> f a -> g a
lowerFF f = unprobed . f . probed
{-# INLINE lowerFF #-}
