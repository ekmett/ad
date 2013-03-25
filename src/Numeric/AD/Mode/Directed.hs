{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Mode.Directed
-- Copyright   :  (c) Edward Kmett 2010-12
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Allows the choice of AD 'Mode' to be specified at the term level for
-- benchmarking or more complicated usage patterns.
-----------------------------------------------------------------------------

module Numeric.AD.Mode.Directed
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
    ) where

import Prelude hiding (reverse)
import Numeric.AD.Types
import Data.Traversable (Traversable)
import qualified Numeric.AD.Mode.Kahn as K
import qualified Numeric.AD.Mode.Forward as F
import qualified Numeric.AD.Mode.Tower as T
import qualified Numeric.AD.Mode.Reverse as R
import qualified Numeric.AD as M
import Data.Ix

data Direction
    = Forward
    | Kahn
    | Reverse
    | Tower
    | Mixed
    deriving (Show, Eq, Ord, Read, Bounded, Enum, Ix)

diff :: Num a => Direction -> (forall m s. Mode m s => m s -> m s) -> a -> a
diff Forward f a = F.diff f a
diff Kahn f a    = K.diff f a
diff Reverse f a = R.diff f a
diff Tower f a   = T.diff f a
diff Mixed f a   = F.diff f a
{-# INLINE diff #-}

diff' :: Num a => Direction -> (forall m s. Mode m s => m s -> m s) -> a -> (a, a)
diff' Forward f a = F.diff' f a
diff' Kahn f a    = K.diff' f a
diff' Reverse f a = R.diff' f a
diff' Tower f a   = T.diff' f a
diff' Mixed f a   = F.diff' f a
{-# INLINE diff' #-}

jacobian :: (Traversable f, Traversable g, Num a) => Direction -> (forall m s. Mode m s => f (m s) -> g (m s)) -> f a -> g (f a)
jacobian Forward f a = F.jacobian f a
jacobian Kahn f a    = K.jacobian f a
jacobian Reverse f a = R.jacobian f a
jacobian Tower f a   = F.jacobian f a -- error "jacobian Tower: unimplemented"
jacobian Mixed f a   = M.jacobian f a
{-# INLINE jacobian #-}

jacobian' :: (Traversable f, Traversable g, Num a) => Direction -> (forall m s. Mode m s => f (m s) -> g (m s)) -> f a -> g (a, f a)
jacobian' Forward f a = F.jacobian' f a
jacobian' Kahn f a    = K.jacobian' f a
jacobian' Reverse f a = R.jacobian' f a
jacobian' Tower f a   = F.jacobian' f a -- error "jacobian' Tower: unimplemented"
jacobian' Mixed f a   = M.jacobian' f a
{-# INLINE jacobian' #-}

grad :: (Traversable f, Num a) => Direction -> (forall m s. Mode m s => f (m s) -> m s) -> f a -> f a
grad Forward f a = F.grad f a
grad Kahn f a    = K.grad f a
grad Reverse f a = R.grad f a
grad Tower f a   = F.grad f a -- error "grad Tower: unimplemented"
grad Mixed f a   = M.grad f a
{-# INLINE grad #-}

grad' :: (Traversable f, Num a) => Direction -> (forall m s. Mode m s => f (m s) -> m s) -> f a -> (a, f a)
grad' Forward f a = F.grad' f a
grad' Kahn f a    = K.grad' f a
grad' Reverse f a = R.grad' f a
grad' Tower f a   = F.grad' f a -- error "grad' Tower: unimplemented"
grad' Mixed f a   = M.grad' f a
{-# INLINE grad' #-}
