{-# LANGUAGE FlexibleContexts, Rank2Types, TypeFamilies, PatternGuards #-}
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

module Numeric.AD
    (
    -- * Gradients (Reverse Mode)
      grad
    , grad'
    , gradWith
    , gradWith'

    -- * Higher Order Gradients (Sparse-on-Reverse)
    , grads

    -- * Jacobians (Sparse or Reverse)
    -- , jacobian
    -- , jacobian'
    -- , jacobianWith
    -- , jacobianWith'

    -- * Higher Order Jacobian (Sparse-on-Reverse)
    , jacobians

    -- * Transposed Jacobians (Forward Mode)
    , jacobianT
    , jacobianWithT

    -- * Hessian (Sparse-On-Reverse)
    -- , hessian
    -- , hessian'

    -- * Hessian Tensors (Sparse or Sparse-On-Reverse)
    -- , hessianF
    -- * Hessian Tensors (Sparse)
    -- , hessianF'

    -- * Hessian Vector Products (Forward-On-Reverse)
    -- , hessianProduct
    -- , hessianProduct'

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
    -- , conjugateGradientDescent
    -- , conjugateGradientAscent
    ) where

import Data.Traversable (Traversable)
import Data.Foldable (Foldable, foldr')
import Data.Reflection (Reifies)
import Control.Applicative

import Numeric.AD.Types
import Numeric.AD.Internal.Composition
import Numeric.AD.Internal.Forward (Forward)
import Numeric.AD.Internal.Identity
import Numeric.AD.Internal.Reverse (Reverse, Tape)
import Numeric.AD.Internal.Sparse (Sparse)

import Numeric.AD.Mode.Forward
    ( diff, diff', diffF, diffF'
    , du, du', duF, duF'
    , jacobianT, jacobianWithT )

import Numeric.AD.Mode.Tower
    ( diffsF, diffs0F, diffs, diffs0
    , taylor, taylor0, maclaurin, maclaurin0
    , dus, dus0, dusF, dus0F )

import qualified Numeric.AD.Mode.Reverse as Reverse
import Numeric.AD.Mode.Reverse
    ( grad, grad', gradWith, gradWith')

-- temporary until we make a full sparse mode
import qualified Numeric.AD.Mode.Sparse as Sparse
import Numeric.AD.Mode.Sparse
    ( grads, jacobians, hessian', hessianF')

import Numeric.AD.Newton

{-
-- | Calculate the Jacobian of a non-scalar-to-non-scalar function, automatically choosing between sparse and reverse mode AD based on the number of inputs and outputs.
--
-- If you know the relative number of inputs and outputs, consider 'Numeric.AD.Reverse.jacobian' or 'Nuneric.AD.Sparse.jacobian'.
jacobian :: (Traversable f, Functor g, Num a) => (forall m s. Mode m => f (AD s m) -> g (AD s m)) -> f a -> g (f a)
jacobian f bs = snd <$> jacobian' f bs
{-# INLINE jacobian #-}

data Nat = Z | S Nat deriving (Eq, Ord)

size :: Foldable f => f a -> Nat
size = foldr' (\_ b -> S b) Z

big :: Nat -> Bool
big (S (S (S (S (S (S (S (S (S (S _)))))))))) = True
big _ = False

-- | Calculate both the answer and Jacobian of a non-scalar-to-non-scalar function, automatically choosing between sparse- and reverse- mode AD based on the relative, based on the number of inputs
--
-- If you know the relative number of inputs and outputs, consider 'Numeric.AD.Reverse.jacobian'' or 'Nuneric.AD.Sparse.jacobian''.
jacobian' :: (Traversable f, Functor g, Num a) => (forall m s. Mode m => f (AD s m) -> g (AD s m)) -> f a -> g (a, f a)
jacobian' f bs | Z <- n = fmap (\x -> (unprobe x, bs)) (f (probed bs))
               | big n  = Reverse.jacobian' f bs
               | otherwise = Sparse.jacobian' f bs
    where
        n = size bs
{-# INLINE jacobian' #-}

-- | @'jacobianWith' g f@ calculates the Jacobian of a non-scalar-to-non-scalar function, automatically choosing between sparse and reverse mode AD based on the number of inputs and outputs.
--
-- The resulting Jacobian matrix is then recombined element-wise with the input using @g@.
--
-- If you know the relative number of inputs and outputs, consider 'Numeric.AD.Reverse.jacobianWith' or 'Nuneric.AD.Sparse.jacobianWith'.
jacobianWith :: (Traversable f, Functor g, Num a) => (a -> a -> b) -> (forall m s. Mode m => f (AD s m) -> g (AD s m)) -> f a -> g (f b)
jacobianWith g f bs = snd <$> jacobianWith' g f bs
{-# INLINE jacobianWith #-}

-- | @'jacobianWith'' g f@ calculates the answer and Jacobian of a non-scalar-to-non-scalar function, automatically choosing between sparse and reverse mode AD based on the number of inputs and outputs.
--
-- The resulting Jacobian matrix is then recombined element-wise with the input using @g@.
--
-- If you know the relative number of inputs and outputs, consider 'Numeric.AD.Reverse.jacobianWith'' or 'Nuneric.AD.Sparse.jacobianWith''.
jacobianWith' :: (Traversable f, Functor g, Num a) => (a -> a -> b) -> (forall m s. Mode m => f (AD s m) -> g (AD s m)) -> f a -> g (a, f b)
jacobianWith' g f bs
    | Z <- n = fmap (\x -> (unprobe x, undefined <$> bs)) (f (probed bs))
    | big n  = Reverse.jacobianWith' g f bs
    | otherwise = Sparse.jacobianWith' g f bs
    where
        n = size bs
{-# INLINE jacobianWith' #-}
-}

{-
-- | @'hessianProduct' f wv@ computes the product of the hessian @H@ of a non-scalar-to-scalar function @f@ at @w = 'fst' <$> wv@ with a vector @v = snd <$> wv@ using \"Pearlmutter\'s method\" from <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.29.6143>, which states:
--
-- > H v = (d/dr) grad_w (w + r v) | r = 0
--
-- Or in other words, we take the directional derivative of the gradient. The gradient is calculated in reverse mode, then the directional derivative is calculated in forward mode.
--
hessianProduct :: (Traversable f, Num a) => (forall s s'. Reifies s Tape => f (AD s (ComposeMode (Reverse s) Forward s' a)) -> AD s (ComposeMode (Reverse s) Forward s' a)) -> f (a, a) -> f a
hessianProduct f = duF (grad (decomposeMode . f . fmap composeMode))

-- | @'hessianProduct'' f wv@ computes both the gradient of a non-scalar-to-scalar @f@ at @w = 'fst' <$> wv@ and the product of the hessian @H@ at @w@ with a vector @v = snd <$> wv@ using \"Pearlmutter's method\". The outputs are returned wrapped in the same functor.
--
-- > H v = (d/dr) grad_w (w + r v) | r = 0
--
-- Or in other words, we return the gradient and the directional derivative of the gradient. The gradient is calculated in reverse mode, then the directional derivative is calculated in forward mode.
hessianProduct' :: (Traversable f, Num a) => (forall s s'. Reifies s Tape => f (AD s (ComposeMode (Reverse s) Forward s' a)) -> AD s (ComposeMode (Reverse s) Forward s' a)) -> f (a, a) -> f (a, a)
hessianProduct' f = duF' (grad (decomposeMode . f . fmap composeMode))

-- | Compute the Hessian via the Jacobian of the gradient. gradient is computed in reverse mode and then the Jacobian is computed in sparse (forward) mode.
--
-- >>> hessian (\[x,y] -> x*y) [1,2]
-- [[0,1],[1,0]]
hessian :: (Traversable f, Num a) => (forall s s'. Reifies s Tape => f (AD s (ComposeMode (Reverse s) Sparse s' a)) -> AD s (ComposeMode (Reverse s) Sparse s' a)) -> f a -> f (f a)
hessian f = Sparse.jacobian (grad (decomposeMode . f . fmap composeMode))

-- | Compute the order 3 Hessian tensor on a non-scalar-to-non-scalar function using 'Sparse' or 'Sparse'-on-'Reverse'
--
-- >>> hessianF (\[x,y] -> [x*y,x+y,exp x*cos y]) [1,2]
-- [[[0.0,1.0],[1.0,0.0]],[[0.0,0.0],[0.0,0.0]],[[-1.1312043837568135,-2.4717266720048188],[-2.4717266720048188,1.1312043837568135]]]
hessianF :: (Traversable f, Functor g, Floating a) => (forall m s. (Mode m, Floating m) => f (AD s m) -> g (AD s m)) -> f a -> g (f (f a))
hessianF f as
    | big (size as) = decomposeFunctor $ Sparse.jacobian (ComposeFunctor . Reverse.jacobian (fmap decomposeMode . f . fmap composeMode)) as
    | otherwise = Sparse.hessianF f as
-}
