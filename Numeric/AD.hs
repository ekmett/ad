{-# LANGUAGE Rank2Types, TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only 
--
-- Mixed-Mode Automatic Differentiation.
-- 
-- Each combinator exported from this module chooses an appropriate AD mode.
-----------------------------------------------------------------------------

module Numeric.AD 
    ( AD(..)
    , Mode(..)

    -- * Derivatives
    -- ** forward-mode
    , diffUU
    , diffUF

    , diff2UU
    , diff2UF

    -- ** reverse-mode
    , diffFU
    , diff2FU

    -- ** forward mode tower
    , diffsUU
    , diffsUF

    , diffs0UU
    , diffs0UF

    -- * Common access patterns
    , diff
    , diff2
    , diffs
    , diffs0

    -- * One-pass reverse-mode gradient
    , grad, grad2

    -- * Self-optimizing Jacobians
    , jacobian, jacobian2

    -- * Taylor series
    , taylor
    , taylor0
    ) where

import Data.Traversable (Traversable, mapM)
import Data.Foldable (Foldable, foldr')
import Control.Applicative
import Numeric.AD.Classes  (Mode(..))
import Numeric.AD.Internal (AD(..), Id(..))
import Numeric.AD.Forward  (diff, diffUU, diff2, diff2UU, diffUF, diff2UF)
import Numeric.AD.Tower    (diffsUU, diffs0UU , diffsUF, diffs0UF , diffs, diffs0, taylor, taylor0) 
import Numeric.AD.Reverse  (diffFU, diff2FU, grad, grad2)

import qualified Numeric.AD.Forward as Forward
import qualified Numeric.AD.Reverse as Reverse

size :: Foldable f => f a -> Int
size = foldr' (\_ b -> 1 + b) 0 

probe :: a -> AD Id a
probe = AD . Id

unprobe :: AD Id a -> a
unprobe (AD (Id a)) = a

-- | Calculate the Jacobian of a non-scalar-to-non-scalar function, automatically choosing between forward and reverse mode AD based on the number of inputs and outputs
jacobian :: (Traversable f, Traversable g, Num a) => (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (f a)
jacobian f bs = snd <$> jacobian2 f bs
{-# INLINE jacobian #-}

-- | Calculate the answer and Jacobian of a non-scalar-to-non-scalar function, automatically choosing between forward and reverse mode AD based on the number of inputs and outputs.
jacobian2 :: (Traversable f, Traversable g, Num a) => (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (a, f a)
jacobian2 f bs | n == 0    = fmap (\x -> (unprobe x, bs)) as
               | n > m     = Reverse.jacobian2 f bs
               | otherwise = Forward.jacobian2 f bs
    where
        n = size bs
        as = f (probe <$> bs)
        m = size as
{-# INLINE jacobian2 #-}
