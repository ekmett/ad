{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2010-2015
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Mixed-Mode Automatic Differentiation.
--
-- Each combinator exported from this module chooses an appropriate AD mode.
-- The following basic operations are supported, modified as appropriate by the suffixes below:
--
-- * 'grad' computes the gradient (partial derivatives) of a function at a point
--
-- * 'jacobian' computes the Jacobian matrix of a function at a point
--
-- * 'diff' computes the derivative of a function at a point
--
-- * 'du' computes a directional derivative of a function at a point
--
-- * 'hessian' compute the Hessian matrix (matrix of second partial derivatives) of a function at a point
--
-- The suffixes have the following meanings:
--
-- * @\'@ -- also return the answer
--
-- * @With@ lets the user supply a function to blend the input with the output
--
-- * @F@ is a version of the base function lifted to return a 'Traversable' (or 'Functor') result
--
-- * @s@ means the function returns all higher derivatives in a list or f-branching 'Stream'
--
-- * @T@ means the result is transposed with respect to the traditional formulation.
--
-- * @0@ means that the resulting derivative list is padded with 0s at the end.
-----------------------------------------------------------------------------

module Numeric.AD.Rank1
  (

  -- * AD modes
    Mode(auto)
  , Scalar

  -- * Gradients (Kahn Mode)
  , grad
  , grad'
  , gradWith
  , gradWith'

  -- * Higher Order Gradients (Sparse-on-Kahn)
  , grads

  -- * Variadic Gradients (Sparse or Kahn)
  -- $vgrad
  , Grad , vgrad, vgrad'
  , Grads, vgrads

  -- * Jacobians (Kahn)
  , jacobian
  , jacobian'
  , jacobianWith
  , jacobianWith'

  -- * Higher Order Jacobian (Sparse-on-Kahn)
  , jacobians

  -- * Transposed Jacobians (Forward Mode)
  , jacobianT
  , jacobianWithT

  -- * Hessian (Sparse-On-Kahn)
  , hessian
  , hessian'

  -- * Hessian Tensors (Sparse or Sparse-On-Kahn)
  , hessianF

  -- * Hessian Tensors (Sparse)
  , hessianF'

  -- * Hessian Vector Products (Forward-On-Kahn)
  , hessianProduct
  , hessianProduct'

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

  -- * Directional Derivatives (Tower)
  , dus
  , dus0
  , dusF
  , dus0F

  -- * Taylor Series (Tower)
  , taylor
  , taylor0

  -- * Maclaurin Series (Tower)
  , maclaurin
  , maclaurin0

  -- * Gradient Descent
  , gradientDescent
  , gradientAscent
  ) where

import Control.Applicative
import Data.Functor.Compose
import Data.Traversable (Traversable)
import Data.Reflection (Reifies)
import Numeric.AD.Internal.Forward (Forward)
import Numeric.AD.Internal.Kahn (Grad, vgrad, vgrad')
import Numeric.AD.Internal.On
import Numeric.AD.Internal.Reverse (Reverse, Tape)
import Numeric.AD.Internal.Sparse (Sparse, Grads, vgrads)

import Numeric.AD.Mode

import Numeric.AD.Rank1.Forward
  ( diff, diff', diffF, diffF'
  , du, du', duF, duF'
  , jacobianT, jacobianWithT )

import Numeric.AD.Rank1.Tower
  ( diffsF, diffs0F, diffs, diffs0
  , taylor, taylor0, maclaurin, maclaurin0
  , dus, dus0, dusF, dus0F )

import qualified Numeric.AD.Rank1.Kahn as Kahn
import Numeric.AD.Rank1.Kahn
  ( Kahn, grad, grad', gradWith, gradWith'
  , jacobian, jacobian', jacobianWith, jacobianWith')

-- temporary until we make a full sparse mode
import qualified Numeric.AD.Rank1.Sparse as Sparse
import Numeric.AD.Rank1.Sparse
  ( grads, jacobians, hessian', hessianF')

import Numeric.AD.Rank1.Newton

-- | @'hessianProduct' f wv@ computes the product of the hessian @H@ of a non-scalar-to-scalar function @f@ at @w = 'fst' '<$>' wv@ with a vector @v = snd '<$>' wv@ using \"Pearlmutter\'s method\" from <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.29.6143>, which states:
--
-- > H v = (d/dr) grad_w (w + r v) | r = 0
--
-- Or in other words, we take the directional derivative of the gradient. The gradient is calculated in reverse mode, then the directional derivative is calculated in forward mode.
--
hessianProduct :: (Traversable f, Num a) => (f (On (Kahn (Forward a))) -> On (Kahn (Forward a))) -> f (a, a) -> f a
hessianProduct f = duF (grad (off . f . fmap On))

-- | @'hessianProduct'' f wv@ computes both the gradient of a non-scalar-to-scalar @f@ at @w = 'fst' '<$>' wv@ and the product of the hessian @H@ at @w@ with a vector @v = snd '<$>' wv@ using \"Pearlmutter's method\". The outputs are returned wrapped in the same functor.
--
-- > H v = (d/dr) grad_w (w + r v) | r = 0
--
-- Or in other words, we return the gradient and the directional derivative of the gradient. The gradient is calculated in reverse mode, then the directional derivative is calculated in forward mode.
hessianProduct' :: (Traversable f, Num a) => (f (On (Kahn (Forward a))) -> On (Kahn (Forward a))) -> f (a, a) -> f (a, a)
hessianProduct' f = duF' (grad (off . f . fmap On))

-- | Compute the Hessian via the Jacobian of the gradient. gradient is computed in reverse mode and then the Jacobian is computed in sparse (forward) mode.
--
-- >>> hessian (\[x,y] -> x*y) [1,2]
-- [[0,1],[1,0]]
hessian :: (Traversable f, Num a) => (f (On (Kahn (Sparse a))) -> On (Kahn (Sparse a))) -> f a -> f (f a)
hessian f = Sparse.jacobian (grad (off . f . fmap On))

-- | Compute the order 3 Hessian tensor on a non-scalar-to-non-scalar function using 'Sparse'-on-'Reverse'
--
-- >>> hessianF (\[x,y] -> [x*y,x+y,exp x*cos y]) [1,2]
-- [[[0.0,1.0],[1.0,0.0]],[[0.0,0.0],[0.0,0.0]],[[-1.1312043837568135,-2.4717266720048188],[-2.4717266720048188,1.1312043837568135]]]
hessianF :: (Traversable f, Functor g, Num a) => (f (On (Kahn (Sparse a))) -> g (On (Kahn (Sparse a)))) -> f a -> g (f (f a))
hessianF f as = getCompose $ Sparse.jacobian (Compose . Kahn.jacobian (fmap off . f . fmap On)) as

-- $vgrad
--
-- Variadic combinators for variadic mixed-mode automatic differentiation.
--
-- Unfortunately, variadicity comes at the expense of being able to use
-- quantification to avoid sensitivity confusion, so be careful when
-- counting the number of 'auto' calls you use when taking the gradient
-- of a function that takes gradients!
