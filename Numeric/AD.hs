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
    , gradWith, gradWith2

    -- * Jacobians
    , jacobian, jacobian2
    , jacobianWith, jacobianWith2

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

    -- * Derivatives (Tower)
    , diffsUU
    , diffsUF

    , diffs0UU
    , diffs0UF

    -- * Directional Derivatives (Forward)
    , diffMU
    , diff2MU
    , diffMF
    , diff2MF

    -- * Taylor Series (Tower)
    , taylor
    , taylor0
    , maclaurin
    , maclaurin0

    -- * Monadic Gradient (wrapped Jacobian)
    , gradM, grad2M
    , gradWithM, gradWith2M

    -- * Exposed Types
    , AD(..)
    , Mode(..)
    ) where

import Data.Traversable (Traversable)
import Data.Foldable (Foldable, foldr')
import Control.Applicative
import Numeric.AD.Classes  (Mode(..))
import Numeric.AD.Internal (AD(..), probed, unprobe)
import Numeric.AD.Forward  (diff, diffUU, diff2, diff2UU, diffUF, diff2UF, diffMU, diff2MU, diffMF, diff2MF)
import Numeric.AD.Tower    (diffsUU, diffs0UU , diffsUF, diffs0UF , diffs, diffs0, taylor, taylor0, maclaurin, maclaurin0)
import Numeric.AD.Reverse  (grad, grad2, gradWith, gradWith2, gradM, grad2M, gradWithM, gradWith2M)

import qualified Numeric.AD.Forward as Forward
import qualified Numeric.AD.Reverse as Reverse

-- | Calculate the Jacobian of a non-scalar-to-non-scalar function, automatically choosing between forward and reverse mode AD based on the number of inputs and outputs.
--
-- If you need to support functions where the output is only a 'Functor' or 'Monad', consider 'Numeric.AD.Reverse.jacobian' or 'Numeric.AD.Reverse.gradM' from "Numeric.AD.Reverse".
jacobian :: (Traversable f, Traversable g, Num a) => (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (f a)
jacobian f bs = snd <$> jacobian2 f bs
{-# INLINE jacobian #-}

-- | Calculate both the answer and Jacobian of a non-scalar-to-non-scalar function, automatically choosing between forward- and reverse- mode AD based on the relative, number of inputs and outputs.
--
-- If you need to support functions where the output is only a 'Functor' or 'Monad', consider 'Numeric.AD.Reverse.jacobian2' or 'Numeric.AD.Reverse.grad2M' from "Numeric.AD.Reverse".
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

-- | @'jacobianWith' g f@ calculates the Jacobian of a non-scalar-to-non-scalar function, automatically choosing between forward and reverse mode AD based on the number of inputs and outputs.
--
-- The resulting Jacobian matrix is then recombined element-wise with the input using @g@.
--
-- If you need to support functions where the output is only a 'Functor' or 'Monad', consider 'Numeric.AD.Reverse.jacobianWith' or 'Numeric.AD.Reverse.gradWithM' from "Numeric.AD.Reverse".
jacobianWith :: (Traversable f, Traversable g, Num a) => (a -> a -> b) -> (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (f b)
jacobianWith g f bs = snd <$> jacobianWith2 g f bs
{-# INLINE jacobianWith #-}

-- | @'jacobianWith2' g f@ calculates the answer and Jacobian of a non-scalar-to-non-scalar function, automatically choosing between forward and reverse mode AD based on the number of inputs and outputs.
--
-- The resulting Jacobian matrix is then recombined element-wise with the input using @g@.
--
-- If you need to support functions where the output is only a 'Functor' or 'Monad', consider 'Numeric.AD.Reverse.jacobianWith2' or 'Numeric.AD.Reverse.gradWith2M' from "Numeric.AD.Reverse".
jacobianWith2 :: (Traversable f, Traversable g, Num a) => (a -> a -> b) -> (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (a, f b)
jacobianWith2 g f bs
    | n == 0    = fmap (\x -> (unprobe x, undefined <$> bs)) as
    | n > m     = Reverse.jacobianWith2 g f bs
    | otherwise = Forward.jacobianWith2 g f bs
    where
        as = f (probed bs)
        n = size bs
        m = size as
        size :: Foldable f => f a -> Int
        size = foldr' (\_ b -> 1 + b) 0
{-# INLINE jacobianWith2 #-}

