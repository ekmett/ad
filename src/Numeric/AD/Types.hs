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
    -- * Jets
    , Jet(..)
    , headJet
    , tailJet
    , jet
    -- * Apply functions that use 'lift'
    , lowerUU, lowerUF, lowerFU, lowerFF
    ) where

import Numeric.AD.Internal.Identity
import Numeric.AD.Internal.Jet
import Numeric.AD.Internal.Classes

-- | Evaluate a scalar-to-scalar function in the trivial identity AD mode.
lowerUU :: (forall s. Id a s -> Id a s) -> a -> a
lowerUU f = unprobe . f . probe
{-# INLINE lowerUU #-}

-- | Evaluate a scalar-to-nonscalar function in the trivial identity AD mode.
lowerUF :: Functor f => (forall s. Id a s -> f (Id a s)) -> a -> f a
lowerUF f = unprobed . f . probe
{-# INLINE lowerUF #-}

-- | Evaluate a nonscalar-to-scalar function in the trivial identity AD mode.
lowerFU :: Functor f => (forall s. f (Id a s) -> Id a s) -> f a -> a
lowerFU f = unprobe . f . probed
{-# INLINE lowerFU #-}

-- | Evaluate a nonscalar-to-nonscalar function in the trivial identity AD mode.
lowerFF :: (Functor f, Functor g) => (forall s. f (Id a s) -> g (Id a s)) -> f a -> g a
lowerFF f = unprobed . f . probed
{-# INLINE lowerFF #-}
