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
    -- * Explicit modes
    , Direction(..)
    -- * Derivatives
    , diffUU
    , diff2UU
    -- * Common access patterns
    , diff
    , diff2
    ) where

import Prelude hiding (reverse)
import Numeric.AD.Classes
import Numeric.AD.Internal
import qualified Numeric.AD.Reverse as R
import qualified Numeric.AD.Forward as F
import qualified Numeric.AD.Tower as T
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
