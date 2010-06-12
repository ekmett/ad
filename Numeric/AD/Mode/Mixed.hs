{-# LANGUAGE Rank2Types, TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Mode.Mixed
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
-- * @M@ is a version of the base function lifted to return a 'Monad'ic result
--
-- * @s@ means the function returns all higher derivatives in a list or f-branching 'Stream'
--
-- * @T@ means the result is transposed with respect to the traditional formulation.
--
-- * @0@ means that the resulting derivative list is padded with 0s at the end.
-----------------------------------------------------------------------------

module Numeric.AD.Mode.Mixed
    (
    -- * Gradients (Reverse Mode)
      grad
    , grad'
    , gradWith
    , gradWith'

    -- * Higher Order Gradients (Sparse-on-Reverse)
    , grads
    , gradsF
    , gradsM

    -- * Jacobians (Mixed Mode)
    , jacobian
    , jacobian'
    , jacobianWith
    , jacobianWith'

    -- * Higher Order Jacobian (Sparse-on-Reverse)
    , jacobians

    -- * Monadic Gradient / Jacobian (Reverse Mode)
    , gradM
    , gradM'
    , gradWithM
    , gradWithM'

    -- * Functor Gradient / Jacobian (Reverse Mode)
    , gradF
    , gradF'
    , gradWithF
    , gradWithF'

    -- * Transposed Jacobians (Forward Mode)
    , jacobianT
    , jacobianWithT

    -- * Hessian (Sparse-On-Reverse)
    , hessian
    , hessian'

    -- * Hessian Tensors (Sparse-On-Mixed)
    , hessianF

    -- * Hessian Tensors (Sparse)
    , hessianF'
    , hessianM 
    , hessianM'

    -- * Hessian Vector Products (Forward-On-Reverse)
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

    -- * Monadic Combinators (Forward Mode)
    , diffM
    , diffM'

    -- * Exposed Types
    , module Numeric.AD.Types
    , Mode(..)
    ) where

import Data.Traversable (Traversable)
import Data.Foldable (Foldable, foldr')
import Control.Applicative

import Numeric.AD.Types
import Numeric.AD.Internal.Composition
import Numeric.AD.Classes (Mode(..))

import qualified Numeric.AD.Mode.Forward as Forward
import Numeric.AD.Mode.Forward 
    ( diff, diff', diffF, diffF'
    , du, du', duF, duF'
    , diffM, diffM'
    , jacobianT, jacobianWithT
    ) 

import Numeric.AD.Mode.Tower 
    ( diffsF, diffs0F, diffs, diffs0
    , taylor, taylor0, maclaurin, maclaurin0
    , dus, dus0, dusF, dus0F
    )

import qualified Numeric.AD.Mode.Reverse as Reverse
import Numeric.AD.Mode.Reverse 
    ( grad, grad', gradWith, gradWith'
    , gradM, gradM', gradWithM, gradWithM'
    , gradF, gradF', gradWithF, gradWithF'
    )

-- temporary until we make a full sparse mode
import qualified Numeric.AD.Mode.Sparse as Sparse
import Numeric.AD.Mode.Sparse
    ( grads, gradsM, gradsF
    , jacobians
    -- , hessian, hessianF
    , hessian', hessianF'
    , hessianM, hessianM' 
    )
    
    

-- | Calculate the Jacobian of a non-scalar-to-non-scalar function, automatically choosing between forward and reverse mode AD based on the number of inputs and outputs.
--
-- If you know the relative number of inputs and outputs, consider 'Numeric.AD.Reverse.jacobian' or 'Nuneric.AD.Sparse.jacobian'.
jacobian :: (Traversable f, Traversable g, Num a) => FF f g a -> f a -> g (f a)
jacobian f bs = snd <$> jacobian' f bs
{-# INLINE jacobian #-}

-- | Calculate both the answer and Jacobian of a non-scalar-to-non-scalar function, automatically choosing between forward- and reverse- mode AD based on the relative, number of inputs and outputs.
--
-- If you know the relative number of inputs and outputs, consider 'Numeric.AD.Reverse.jacobian'' or 'Nuneric.AD.Sparse.jacobian''.
jacobian' :: (Traversable f, Traversable g, Num a) => FF f g a -> f a -> g (a, f a)
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
-- If you know the relative number of inputs and outputs, consider 'Numeric.AD.Reverse.jacobianWith' or 'Nuneric.AD.Sparse.jacobianWith'.
jacobianWith :: (Traversable f, Traversable g, Num a) => (a -> a -> b) -> FF f g a -> f a -> g (f b)
jacobianWith g f bs = snd <$> jacobianWith' g f bs
{-# INLINE jacobianWith #-}

-- | @'jacobianWith'' g f@ calculates the answer and Jacobian of a non-scalar-to-non-scalar function, automatically choosing between sparse and reverse mode AD based on the number of inputs and outputs.
--
-- The resulting Jacobian matrix is then recombined element-wise with the input using @g@.
--
-- If you know the relative number of inputs and outputs, consider 'Numeric.AD.Reverse.jacobianWith'' or 'Nuneric.AD.Sparse.jacobianWith''.
jacobianWith' :: (Traversable f, Traversable g, Num a) => (a -> a -> b) -> FF f g a -> f a -> g (a, f b)
jacobianWith' g f bs
    | n == 0    = fmap (\x -> (unprobe x, undefined <$> bs)) as
    | n > m     = Reverse.jacobianWith' g f bs
    | otherwise = Sparse.jacobianWith' g f bs
    where
        as = f (probed bs)
        n = size bs
        m = size as
        size :: Foldable f => f a -> Int
        size = foldr' (\_ b -> 1 + b) 0
{-# INLINE jacobianWith' #-}

-- | @'hessianProduct' f wv@ computes the product of the hessian @H@ of a non-scalar-to-scalar function @f@ at @w = 'fst' <$> wv@ with a vector @v = snd <$> wv@ using \"Pearlmutter\'s method\" from <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.29.6143>, which states:
--
-- > H v = (d/dr) grad_w (w + r v) | r = 0
-- 
-- Or in other words, we take the directional derivative of the gradient. The gradient is calculated in reverse mode, then the directional derivative is calculated in forward mode.
--
hessianProduct :: (Traversable f, Num a) => FU f a -> f (a, a) -> f a
hessianProduct f = duF (grad (decomposeMode . f . fmap composeMode))

-- | @'hessianProduct'' f wv@ computes both the gradient of a non-scalar-to-scalar @f@ at @w = 'fst' <$> wv@ and the product of the hessian @H@ at @w@ with a vector @v = snd <$> wv@ using \"Pearlmutter's method\". The outputs are returned wrapped in the same functor.
--
-- > H v = (d/dr) grad_w (w + r v) | r = 0
-- 
-- Or in other words, we return the gradient and the directional derivative of the gradient. The gradient is calculated in reverse mode, then the directional derivative is calculated in forward mode.
hessianProduct' :: (Traversable f, Num a) => FU f a -> f (a, a) -> f (a, a)
hessianProduct' f = duF' (grad (decomposeMode . f . fmap composeMode))

-- | Compute the Hessian via the Jacobian of the gradient. gradient is computed in reverse mode and then the Jacobian is computed in sparse (forward) mode.
hessian :: (Traversable f, Num a) => FU f a -> f a -> f (f a)
hessian f = Sparse.jacobian (grad (decomposeMode . f . fmap composeMode))

-- | Compute the order 3 Hessian tensor on a non-scalar-to-non-scalar function via the Sparse Jacobian of the (sparse-or-reverse)-mode Jacobian of the function.
hessianF :: (Traversable f, Traversable g, Num a) => FF f g a -> f a -> g (f (f a))
hessianF f = decomposeFunctor . Sparse.jacobian (ComposeFunctor . jacobian (fmap decomposeMode . f . fmap composeMode))
