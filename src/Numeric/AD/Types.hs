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
lowerUU :: (forall s. Mode s => AD s a -> AD s a) -> a -> a
lowerUU f = unprobe . f . probe
{-# INLINE lowerUU #-}

-- | Evaluate a scalar-to-nonscalar function in the trivial identity AD mode.
lowerUF :: (forall s. Mode s => AD s a -> f (AD s a)) -> a -> f a
lowerUF f = unprobed . f . probe
{-# INLINE lowerUF #-}

-- | Evaluate a nonscalar-to-scalar function in the trivial identity AD mode.
lowerFU :: (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> a
lowerFU f = unprobe . f . probed
{-# INLINE lowerFU #-}

-- | Evaluate a nonscalar-to-nonscalar function in the trivial identity AD mode.
lowerFF :: (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g a
lowerFF f = unprobed . f . probed
{-# INLINE lowerFF #-}
