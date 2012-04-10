{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Types
-- Copyright   :  (c) Edward Kmett 2010
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
    -- * Tensors
    , Tensors(..)
    , headT
    , tailT
    , tensors
    -- * Apply functions that use 'lift'
    , lowerUU, lowerUF, lowerFU, lowerFF
    ) where

import Numeric.AD.Internal.Identity
import Numeric.AD.Internal.Types
import Numeric.AD.Internal.Tensors
import Numeric.AD.Internal.Classes

-- these exploit the 'magic' that is probed to avoid the need for Functor, etc.

lowerUU :: (forall s. Mode s => AD s a -> AD s a) -> a -> a
lowerUU f = unprobe . f . probe
{-# INLINE lowerUU #-}

lowerUF :: (forall s. Mode s => AD s a -> f (AD s a)) -> a -> f a
lowerUF f = unprobed . f . probe
{-# INLINE lowerUF #-}

lowerFU :: (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> a
lowerFU f = unprobe . f . probed
{-# INLINE lowerFU #-}

lowerFF :: (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g a
lowerFF f = unprobed . f . probed
{-# INLINE lowerFF #-}
