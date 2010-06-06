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
    ( 
    -- * Gradients
      grad, grad2

    -- * Jacobians
    , jacobian, jacobian2

    -- * Synonyms
    , diff
    , diff2
    , diffs
    , diffs0

    -- * Derivatives (Forward)
    , diffUU
    , diffUF

    , diff2UU
    , diff2UF

    -- * Derivatives (Reverse)
    , diffFU
    , diff2FU

    -- * Derivatives (Tower)
    , diffsUU
    , diffsUF

    , diffs0UU
    , diffs0UF

    -- * Taylor Series (Tower)
    , taylor
    , taylor0

    -- * Exposed Types
    , AD(..)
    , Mode(..)
    ) where

import Data.Traversable (Traversable)
import Data.Foldable (Foldable, foldr')
import Control.Applicative
import Numeric.AD.Classes  (Mode(..))
import Numeric.AD.Internal (AD(..), probed, unprobe)
import Numeric.AD.Forward  (diff, diffUU, diff2, diff2UU, diffUF, diff2UF)
import Numeric.AD.Tower    (diffsUU, diffs0UU , diffsUF, diffs0UF , diffs, diffs0, taylor, taylor0) 
import Numeric.AD.Reverse  (diffFU, diff2FU, grad, grad2)

import qualified Numeric.AD.Forward as Forward
import qualified Numeric.AD.Reverse as Reverse

-- | Calculate the Jacobian of a non-scalar-to-non-scalar function, automatically choosing between forward and reverse mode AD based on the number of inputs and outputs
jacobian :: (Traversable f, Traversable g, Num a) => (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (f a)
jacobian f bs = snd <$> jacobian2 f bs
{-# INLINE jacobian #-}

-- | Calculate the answer and Jacobian of a non-scalar-to-non-scalar function, automatically choosing between forward- and reverse- mode AD based on the relative, number of inputs and outputs. If you need to support functions where the output is only a 'Functor', consider using 'jacobianT' from "Numeric.AD.Forward" or 'jacobian2' from "Numeric.AD.Reverse" directly.
jacobian2 :: (Traversable f, Traversable g, Num a) => (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (a, f a)
jacobian2 f bs | n == 0    = fmap (\x -> (unprobe x, bs)) as
               | n > m     = Reverse.jacobian2 f bs
               | otherwise = Forward.jacobian2 f bs
    where
        as = f (probed bs)
        n = size bs
        m = size as
        size :: Foldable f => f a -> Int
        size = foldr' (\_ b -> 1 + b) 0 
{-# INLINE jacobian2 #-}
