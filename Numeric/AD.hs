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
    -- * Gradients (Reverse Mode)
      grad, grad'
    , gradWith, gradWith'

    -- * Jacobians (Mixed Mode)
    , jacobian, jacobian'
    , jacobianWith, jacobianWith'

    -- * Jacobians (Reverse Mode)
    , gradF
    , gradF'
    , gradWithF
    , gradWithF'

    -- * Jacobians (Forward Mode)
    , jacobianT, jacobianWithT

    -- * Derivatives (Forward Mode)
    , diff
    , diffF

    , diff'
    , diffF'

    -- * Derivatives (Tower)
    , diffs
    , diffsF

    , diffs0
    , diffs0F

    -- * Directional Derivatives (Forward Mode)
    , du
    , du'
    , duF
    , duF'

    -- * Taylor Series (Tower)
    , taylor
    , taylor0

    -- * Maclaurin Series (Tower)
    , maclaurin
    , maclaurin0

    -- * Monadic Combinators (Forward Mode)
    , diffM
    , diffM'

    -- * Monadic Combinators (Reverse Mode)
    , gradM
    , gradM'
    , gradWithM
    , gradWithM'

    -- * Exposed Types
    , AD(..)
    , Mode(..)
    ) where

import Data.Traversable (Traversable)
import Data.Foldable (Foldable, foldr')
import Control.Applicative
import Numeric.AD.Classes  (Mode(..))
import Numeric.AD.Internal (AD(..), probed, unprobe)
import Numeric.AD.Forward  (diff, diff', diffF, diffF', du, du', duF, duF', diffM, diffM', jacobianT, jacobianWithT) 
import Numeric.AD.Tower    (diffsF, diffs0F , diffs, diffs0, taylor, taylor0, maclaurin, maclaurin0)
import Numeric.AD.Reverse  (grad, grad', gradWith, gradWith', gradM, gradM', gradWithM, gradWithM', gradF, gradF', gradWithF, gradWithF')

import qualified Numeric.AD.Forward as Forward
import qualified Numeric.AD.Reverse as Reverse

-- | Calculate the Jacobian of a non-scalar-to-non-scalar function, automatically choosing between forward and reverse mode AD based on the number of inputs and outputs.
--
-- If you need to support functions where the output is only a 'Functor' or 'Monad', consider 'Numeric.AD.Reverse.jacobian' or 'Numeric.AD.Reverse.gradM' from "Numeric.AD.Reverse".
jacobian :: (Traversable f, Traversable g, Num a) => (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (f a)
jacobian f bs = snd <$> jacobian' f bs
{-# INLINE jacobian #-}

-- | Calculate both the answer and Jacobian of a non-scalar-to-non-scalar function, automatically choosing between forward- and reverse- mode AD based on the relative, number of inputs and outputs.
--
-- If you need to support functions where the output is only a 'Functor' or 'Monad', consider 'Numeric.AD.Reverse.jacobian'' or 'Numeric.AD.Reverse.gradM'' from "Numeric.AD.Reverse".
jacobian' :: (Traversable f, Traversable g, Num a) => (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (a, f a)
jacobian' f bs | n == 0    = fmap (\x -> (unprobe x, bs)) as
               | n > m     = Reverse.jacobian' f bs
               | otherwise = Forward.jacobian' f bs
    where
        as = f (probed bs)
        n = size bs
        m = size as
        size :: Foldable f => f a -> Int
        size = foldr' (\_ b -> 1 + b) 0
{-# INLINE jacobian' #-}

-- | @'jacobianWith' g f@ calculates the Jacobian of a non-scalar-to-non-scalar function, automatically choosing between forward and reverse mode AD based on the number of inputs and outputs.
--
-- The resulting Jacobian matrix is then recombined element-wise with the input using @g@.
--
-- If you need to support functions where the output is only a 'Functor' or 'Monad', consider 'Numeric.AD.Reverse.jacobianWith' or 'Numeric.AD.Reverse.gradWithM' from "Numeric.AD.Reverse".
jacobianWith :: (Traversable f, Traversable g, Num a) => (a -> a -> b) -> (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (f b)
jacobianWith g f bs = snd <$> jacobianWith' g f bs
{-# INLINE jacobianWith #-}

-- | @'jacobianWith'' g f@ calculates the answer and Jacobian of a non-scalar-to-non-scalar function, automatically choosing between forward and reverse mode AD based on the number of inputs and outputs.
--
-- The resulting Jacobian matrix is then recombined element-wise with the input using @g@.
--
-- If you need to support functions where the output is only a 'Functor' or 'Monad', consider 'Numeric.AD.Reverse.jacobianWith'' or 'Numeric.AD.Reverse.gradWithM'' from "Numeric.AD.Reverse".
jacobianWith' :: (Traversable f, Traversable g, Num a) => (a -> a -> b) -> (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (a, f b)
jacobianWith' g f bs
    | n == 0    = fmap (\x -> (unprobe x, undefined <$> bs)) as
    | n > m     = Reverse.jacobianWith' g f bs
    | otherwise = Forward.jacobianWith' g f bs
    where
        as = f (probed bs)
        n = size bs
        m = size as
        size :: Foldable f => f a -> Int
        size = foldr' (\_ b -> 1 + b) 0
{-# INLINE jacobianWith' #-}

