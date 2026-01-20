-----------------------------------------------------------------------------
-- |
-- Copyright   : (c) Edward Kmett 2010-2026
-- License     : BSD3
-- Maintainer  : ekmett@gmail.com
-- Stability   : experimental
-- Portability : GHC only
--
-- Higher order derivatives via a \"dual number tower\".
--
-----------------------------------------------------------------------------

module Numeric.AD.Rank1.Sparse.Double
  ( SparseDouble
  , auto
  -- * Sparse Gradients
  , grad
  , grad'
  , gradWith
  , gradWith'
  -- * Variadic Gradients
  -- $vgrad
  , Grad
  , vgrad
  -- * Higher-Order Gradients
  , grads
  -- * Variadic Higher-Order Gradients
  , Grads
  , vgrads

  -- * Sparse Jacobians (synonyms)
  , jacobian
  , jacobian'
  , jacobianWith
  , jacobianWith'
  , jacobians

  -- * Sparse Hessians
  , hessian
  , hessian'

  , hessianF
  , hessianF'

  ) where

import Control.Comonad
import Control.Comonad.Cofree
import Numeric.AD.Jet
import Numeric.AD.Internal.Sparse.Double
import Numeric.AD.Internal.Combinators
import Numeric.AD.Mode

second :: (a -> b) -> (c, a) -> (c, b)
second g (a,b) = (a, g b)
{-# INLINE second #-}

grad
  :: Traversable f
  => (f SparseDouble -> SparseDouble)
  -> f Double -> f Double
grad f as = d as $ apply f as
{-# INLINE grad #-}

grad'
  :: Traversable f
  => (f SparseDouble -> SparseDouble)
  -> f Double -> (Double, f Double)
grad' f as = d' as $ apply f as
{-# INLINE grad' #-}

gradWith
  :: Traversable f
  => (Double -> Double -> b)
  -> (f SparseDouble -> SparseDouble)
  -> f Double
  -> f b
gradWith g f as = zipWithT g as $ grad f as
{-# INLINE gradWith #-}

gradWith'
  :: Traversable f
  => (Double -> Double -> b)
  -> (f SparseDouble -> SparseDouble)
  -> f Double
  -> (Double, f b)
gradWith' g f as = second (zipWithT g as) $ grad' f as
{-# INLINE gradWith' #-}

jacobian
  :: (Traversable f, Functor g)
  => (f SparseDouble -> g SparseDouble)
  -> f Double
  -> g (f Double)
jacobian f as = d as <$> apply f as
{-# INLINE jacobian #-}

jacobian'
  :: (Traversable f, Functor g)
  => (f SparseDouble -> g SparseDouble)
  -> f Double
  -> g (Double, f Double)
jacobian' f as = d' as <$> apply f as
{-# INLINE jacobian' #-}

jacobianWith
  :: (Traversable f, Functor g)
  => (Double -> Double -> b)
  -> (f SparseDouble -> g SparseDouble)
  -> f Double
  -> g (f b)
jacobianWith g f as = zipWithT g as <$> jacobian f as
{-# INLINE jacobianWith #-}

jacobianWith'
  :: (Traversable f, Functor g)
  => (Double -> Double -> b)
  -> (f SparseDouble -> g SparseDouble)
  -> f Double
  -> g (Double, f b)
jacobianWith' g f as = second (zipWithT g as) <$> jacobian' f as
{-# INLINE jacobianWith' #-}

grads
  :: Traversable f
  => (f SparseDouble -> SparseDouble)
  -> f Double
  -> Cofree f Double
grads f as = ds as $ apply f as
{-# INLINE grads #-}

jacobians
  :: (Traversable f, Functor g)
  => (f SparseDouble -> g SparseDouble)
  -> f Double
  -> g (Cofree f Double)
jacobians f as = ds as <$> apply f as
{-# INLINE jacobians #-}

d2 :: Functor f
  => Cofree f a
  -> f (f a)
d2 = headJet . tailJet . tailJet . jet
{-# INLINE d2 #-}

d2'
  :: Functor f
  => Cofree f a
  -> (a, f (a, f a))
d2' (a :< as) = (a, fmap (\(da :< das) -> (da, extract <$> das)) as)
{-# INLINE d2' #-}

hessian
  :: Traversable f
  => (f SparseDouble -> SparseDouble)
  -> f Double
  -> f (f Double)
hessian f as = d2 $ grads f as
{-# INLINE hessian #-}

hessian'
  :: Traversable f
  => (f SparseDouble -> SparseDouble)
  -> f Double
  -> (Double, f (Double, f Double))
hessian' f as = d2' $ grads f as
{-# INLINE hessian' #-}

hessianF
  :: (Traversable f, Functor g)
  => (f SparseDouble -> g SparseDouble)
  -> f Double
  -> g (f (f Double))
hessianF f as = d2 <$> jacobians f as
{-# INLINE hessianF #-}

hessianF'
  :: (Traversable f, Functor g)
  => (f SparseDouble -> g SparseDouble)
  -> f Double
  -> g (Double, f (Double, f Double))
hessianF' f as = d2' <$> jacobians f as
{-# INLINE hessianF' #-}

-- $vgrad
--
-- Variadic combinators for variadic mixed-mode automatic differentiation.
--
-- Unfortunately, variadicity comes at the expense of being able to use
-- quantification to avoid sensitivity confusion, so be careful when
-- counting the number of 'auto' calls you use when taking the gradient
-- of a function that takes gradients!
