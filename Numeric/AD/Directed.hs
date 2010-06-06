{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Directed
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Allows the choice of AD 'Mode' to be specified at the term level for
-- benchmarking or more complicated usage patterns.
-----------------------------------------------------------------------------

module Numeric.AD.Directed
    (
    -- * Gradients
      grad
    , grad'
    -- * Jacobians
    , jacobian
    , jacobian'
    -- * Derivatives
    , diff
    , diff'
    -- * Exposed Types
    , Direction(..)
    , Mode(..)
    , AD(..)
    ) where

import Prelude hiding (reverse)
import Numeric.AD.Classes
import Numeric.AD.Internal
import Data.Traversable (Traversable)
import qualified Numeric.AD.Reverse as R
import qualified Numeric.AD.Forward as F
import qualified Numeric.AD.Tower as T
import qualified Numeric.AD as M
import Data.Ix

-- TODO: use a data types a la carte approach, so we can expose more methods here
-- rather than just the intersection of all of the functionality
data Direction
    = Forward
    | Reverse
    | Tower
    | Mixed
    deriving (Show, Eq, Ord, Read, Bounded, Enum, Ix)

diff :: Num a => Direction -> (forall s. Mode s => AD s a -> AD s a) -> a -> a
diff Forward = F.diff
diff Reverse = R.diff
diff Tower = T.diff
diff Mixed = F.diff
{-# INLINE diff #-}

diff' :: Num a => Direction -> (forall s. Mode s => AD s a -> AD s a) -> a -> (a, a)
diff' Forward = F.diff'
diff' Reverse = R.diff'
diff' Tower = T.diff'
diff' Mixed = F.diff'
{-# INLINE diff' #-}

jacobian :: (Traversable f, Traversable g, Num a) => Direction -> (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (f a)
jacobian Forward = F.jacobian
jacobian Reverse = R.jacobian
jacobian Tower = F.jacobian -- error "jacobian Tower: unimplemented"
jacobian Mixed = M.jacobian
{-# INLINE jacobian #-}

jacobian' :: (Traversable f, Traversable g, Num a) => Direction -> (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (a, f a)
jacobian' Forward = F.jacobian'
jacobian' Reverse = R.jacobian'
jacobian' Tower = F.jacobian' -- error "jacobian' Tower: unimplemented"
jacobian' Mixed = M.jacobian'
{-# INLINE jacobian' #-}

grad :: (Traversable f, Num a) => Direction -> (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> f a
grad Forward = F.grad
grad Reverse = R.grad
grad Tower   = F.grad -- error "grad Tower: unimplemented"
grad Mixed   = M.grad
{-# INLINE grad #-}

grad' :: (Traversable f, Num a) => Direction -> (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> (a, f a)
grad' Forward = F.grad'
grad' Reverse = R.grad'
grad' Tower   = F.grad' -- error "grad' Tower: unimplemented"
grad' Mixed   = M.grad'
{-# INLINE grad' #-}

