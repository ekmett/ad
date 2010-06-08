{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Mode.Directed
-- Copyright   :  (c) Edward Kmett 2010
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
    , UU, UF, FU, FF
    , Direction(..)
    , Mode(..)
    , AD(..)
    ) where

import Prelude hiding (reverse)
import Numeric.AD.Types
import Numeric.AD.Classes
import Data.Traversable (Traversable)
import qualified Numeric.AD.Mode.Reverse as R
import qualified Numeric.AD.Mode.Forward as F
import qualified Numeric.AD.Mode.Tower as T
import qualified Numeric.AD.Mode.Mixed as M
import Data.Ix

-- TODO: use a data types a la carte approach, so we can expose more methods here
-- rather than just the intersection of all of the functionality
data Direction
    = Forward
    | Reverse
    | Tower
    | Mixed
    deriving (Show, Eq, Ord, Read, Bounded, Enum, Ix)

diff :: Num a => Direction -> UU a -> a -> a
diff Forward = F.diff
diff Reverse = R.diff
diff Tower = T.diff
diff Mixed = F.diff
{-# INLINE diff #-}

diff' :: Num a => Direction -> UU a -> a -> (a, a)
diff' Forward = F.diff'
diff' Reverse = R.diff'
diff' Tower = T.diff'
diff' Mixed = F.diff'
{-# INLINE diff' #-}

jacobian :: (Traversable f, Traversable g, Num a) => Direction -> FF f g a -> f a -> g (f a)
jacobian Forward = F.jacobian
jacobian Reverse = R.jacobian
jacobian Tower = F.jacobian -- error "jacobian Tower: unimplemented"
jacobian Mixed = M.jacobian
{-# INLINE jacobian #-}

jacobian' :: (Traversable f, Traversable g, Num a) => Direction -> FF f g a -> f a -> g (a, f a)
jacobian' Forward = F.jacobian'
jacobian' Reverse = R.jacobian'
jacobian' Tower = F.jacobian' -- error "jacobian' Tower: unimplemented"
jacobian' Mixed = M.jacobian'
{-# INLINE jacobian' #-}

grad :: (Traversable f, Num a) => Direction -> FU f a -> f a -> f a
grad Forward = F.grad
grad Reverse = R.grad
grad Tower   = F.grad -- error "grad Tower: unimplemented"
grad Mixed   = M.grad
{-# INLINE grad #-}

grad' :: (Traversable f, Num a) => Direction -> FU f a -> f a -> (a, f a)
grad' Forward = F.grad'
grad' Reverse = R.grad'
grad' Tower   = F.grad' -- error "grad' Tower: unimplemented"
grad' Mixed   = M.grad'
{-# INLINE grad' #-}

