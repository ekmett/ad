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
    ( Mode(..)
    , AD(..)
    , Direction(..)
    -- * Derivatives
    , diffUU
    , diff2UU
    -- * Common access patterns
    , diff
    , diff2
    -- * Jacobians
    , jacobian
    , jacobian2
    -- * Gradients
    , grad
    , grad2
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

diffUU :: Num a => Direction -> (forall s. Mode s => AD s a -> AD s a) -> a -> a
diffUU Forward = F.diffUU
diffUU Reverse = R.diffUU
diffUU Tower = T.diffUU
diffUU Mixed = F.diffUU
{-# INLINE diffUU #-}

diff2UU :: Num a => Direction -> (forall s. Mode s => AD s a -> AD s a) -> a -> (a, a) 
diff2UU Forward = F.diff2UU
diff2UU Reverse = R.diff2UU
diff2UU Tower = T.diff2UU
diff2UU Mixed = F.diff2UU
{-# INLINE diff2UU #-}

diff :: Num a => Direction -> (forall s. Mode s => AD s a -> AD s a) -> a -> a
diff = diffUU
{-# INLINE diff #-}

diff2 :: Num a => Direction -> (forall s. Mode s => AD s a -> AD s a) -> a -> (a, a) 
diff2 = diff2UU
{-# INLINE diff2 #-}

jacobian :: (Traversable f, Traversable g, Num a) => Direction -> (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (f a)
jacobian Forward = F.jacobian
jacobian Reverse = R.jacobian
jacobian Tower = error "jacobian Tower: unimplemented"
jacobian Mixed = M.jacobian
{-# INLINE jacobian #-}

jacobian2 :: (Traversable f, Traversable g, Num a) => Direction -> (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (a, f a)
jacobian2 Forward = F.jacobian2
jacobian2 Reverse = R.jacobian2
jacobian2 Tower = error "jacobian2 Tower: unimplemented"
jacobian2 Mixed = M.jacobian2
{-# INLINE jacobian2 #-}

grad :: (Traversable f, Num a) => Direction -> (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> f a
grad Forward = F.grad
grad Reverse = R.grad
grad Tower   = error "grad Tower: unimplemented"
grad Mixed   = M.grad
{-# INLINE grad #-}

grad2 :: (Traversable f, Num a) => Direction -> (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> (a, f a)
grad2 Forward = F.grad2
grad2 Reverse = R.grad2
grad2 Tower   = error "grad2 Tower: unimplemented"
grad2 Mixed   = M.grad2
{-# INLINE grad2 #-}
