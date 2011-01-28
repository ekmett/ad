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
      AD(..)
    -- * Differentiable Functions
    , UU, UF, FU, FF
    -- * Tensors
    , Tensors(..)
    , headT
    , tailT
    , tensors
    -- * An Identity Mode. 
    , Id(..)
    , probe, unprobe
    , probed, unprobed
    -- * Apply functions that use 'lift'
    , lowerUU, lowerUF, lowerFU, lowerFF
    ) where

import Numeric.AD.Internal.Identity
import Numeric.AD.Internal.Types
import Numeric.AD.Internal.Tensors

-- these exploit the 'magic' that is probed to avoid the need for Functor, etc.

lowerUU :: UU a -> a -> a
lowerUU f = unprobe . f . probe
{-# INLINE lowerUU #-}

lowerUF :: UF f a -> a -> f a
lowerUF f = unprobed . f . probe
{-# INLINE lowerUF #-}

lowerFU :: FU f a -> f a -> a
lowerFU f = unprobe . f . probed
{-# INLINE lowerFU #-}

lowerFF :: FF f g a -> f a -> g a
lowerFF f = unprobed . f . probed
{-# INLINE lowerFF #-}
