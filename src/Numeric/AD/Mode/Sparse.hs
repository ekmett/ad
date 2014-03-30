{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   : (c) Edward Kmett 2010-2014
-- License     : BSD3
-- Maintainer  : ekmett@gmail.com
-- Stability   : experimental
-- Portability : GHC only
--
-- Higher order derivatives via a \"dual number tower\".
--
-----------------------------------------------------------------------------

module Numeric.AD.Mode.Sparse
  ( Sparse
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
import Data.Traversable
import Control.Comonad.Cofree
import Numeric.AD.Jet
import Numeric.AD.Internal.Sparse
import Numeric.AD.Internal.Combinators

second :: (a -> b) -> (c, a) -> (c, b)
second g (a,b) = (a, g b)
{-# INLINE second #-}

grad :: (Traversable f, Num a) => (forall s. f (Sparse a s) -> Sparse a s) -> f a -> f a
grad f as = d as $ apply f as
{-# INLINE grad #-}

grad' :: (Traversable f, Num a) => (forall s. f (Sparse a s) -> Sparse a s) -> f a -> (a, f a)
grad' f as = d' as $ apply f as
{-# INLINE grad' #-}

gradWith :: (Traversable f, Num a) => (a -> a -> b) -> (forall s. f (Sparse a s) -> Sparse a s) -> f a -> f b
gradWith g f as = zipWithT g as $ grad f as
{-# INLINE gradWith #-}

gradWith' :: (Traversable f, Num a) => (a -> a -> b) -> (forall s. f (Sparse a s) -> Sparse a s) -> f a -> (a, f b)
gradWith' g f as = second (zipWithT g as) $ grad' f as
{-# INLINE gradWith' #-}

jacobian :: (Traversable f, Functor g, Num a) => (forall s. f (Sparse a s) -> g (Sparse a s)) -> f a -> g (f a)
jacobian f as = d as <$> apply f as
{-# INLINE jacobian #-}

jacobian' :: (Traversable f, Functor g, Num a) => (forall s. f (Sparse a s) -> g (Sparse a s)) -> f a -> g (a, f a)
jacobian' f as = d' as <$> apply f as
{-# INLINE jacobian' #-}

jacobianWith :: (Traversable f, Functor g, Num a) => (a -> a -> b) -> (forall s. f (Sparse a s) -> g (Sparse a s)) -> f a -> g (f b)
jacobianWith g f as = zipWithT g as <$> jacobian f as
{-# INLINE jacobianWith #-}

jacobianWith' :: (Traversable f, Functor g, Num a) => (a -> a -> b) -> (forall s. f (Sparse a s) -> g (Sparse a s)) -> f a -> g (a, f b)
jacobianWith' g f as = second (zipWithT g as) <$> jacobian' f as
{-# INLINE jacobianWith' #-}

grads :: (Traversable f, Num a) => (forall s. f (Sparse a s) -> Sparse a s) -> f a -> Cofree f a
grads f as = ds as $ apply f as
{-# INLINE grads #-}

jacobians :: (Traversable f, Functor g, Num a) => (forall s. f (Sparse a s) -> g (Sparse a s)) -> f a -> g (Cofree f a)
jacobians f as = ds as <$> apply f as
{-# INLINE jacobians #-}

d2 :: Functor f => Cofree f a -> f (f a)
d2 = headJet . tailJet . tailJet . jet
{-# INLINE d2 #-}

d2' :: Functor f => Cofree f a -> (a, f (a, f a))
d2' (a :< as) = (a, fmap (\(da :< das) -> (da, extract <$> das)) as)
{-# INLINE d2' #-}

hessian :: (Traversable f, Num a) => (forall s. f (Sparse a s) -> Sparse a s) -> f a -> f (f a)
hessian f as = d2 $ grads f as
{-# INLINE hessian #-}

hessian' :: (Traversable f, Num a) => (forall s. f (Sparse a s) -> Sparse a s) -> f a -> (a, f (a, f a))
hessian' f as = d2' $ grads f as
{-# INLINE hessian' #-}

hessianF :: (Traversable f, Functor g, Num a) => (forall s. f (Sparse a s) -> g (Sparse a s)) -> f a -> g (f (f a))
hessianF f as = d2 <$> jacobians f as
{-# INLINE hessianF #-}

hessianF' :: (Traversable f, Functor g, Num a) => (forall s. f (Sparse a s) -> g (Sparse a s)) -> f a -> g (a, f (a, f a))
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
