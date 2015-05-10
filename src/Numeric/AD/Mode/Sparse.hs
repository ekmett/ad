{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   : (c) Edward Kmett 2010-2015
-- License     : BSD3
-- Maintainer  : ekmett@gmail.com
-- Stability   : experimental
-- Portability : GHC only
--
-- Higher order derivatives via a \"dual number tower\".
--
-----------------------------------------------------------------------------

module Numeric.AD.Mode.Sparse
  ( AD, Sparse, auto
  -- * Sparse Gradients
  , grad
  , grad'
  , grads
  , gradWith
  , gradWith'

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

import Control.Comonad.Cofree (Cofree)
import Data.Traversable (Traversable)
import Numeric.AD.Internal.Sparse (Sparse)
import qualified Numeric.AD.Rank1.Sparse as Rank1
import Numeric.AD.Internal.Type
import Numeric.AD.Mode


grad :: (Traversable f, Num a) => (forall s. f (AD s (Sparse a)) -> AD s (Sparse a)) -> f a -> f a
grad f = Rank1.grad (runAD.f.fmap AD)
{-# INLINE grad #-}

grad' :: (Traversable f, Num a) => (forall s. f (AD s (Sparse a)) -> AD s (Sparse a)) -> f a -> (a, f a)
grad' f = Rank1.grad' (runAD.f.fmap AD)
{-# INLINE grad' #-}

gradWith :: (Traversable f, Num a) => (a -> a -> b) -> (forall s. f (AD s (Sparse a)) -> AD s (Sparse a)) -> f a -> f b
gradWith g f = Rank1.gradWith g (runAD.f.fmap AD)
{-# INLINE gradWith #-}

gradWith' :: (Traversable f, Num a) => (a -> a -> b) -> (forall s. f (AD s (Sparse a)) -> AD s (Sparse a)) -> f a -> (a, f b)
gradWith' g f = Rank1.gradWith' g (runAD.f.fmap AD)
{-# INLINE gradWith' #-}

jacobian :: (Traversable f, Functor g, Num a) => (forall s. f (AD s (Sparse a)) -> g (AD s (Sparse a))) -> f a -> g (f a)
jacobian f = Rank1.jacobian (fmap runAD.f.fmap AD)
{-# INLINE jacobian #-}

jacobian' :: (Traversable f, Functor g, Num a) => (forall s. f (AD s (Sparse a)) -> g (AD s (Sparse a))) -> f a -> g (a, f a)
jacobian' f = Rank1.jacobian' (fmap runAD.f.fmap AD)
{-# INLINE jacobian' #-}

jacobianWith :: (Traversable f, Functor g, Num a) => (a -> a -> b) -> (forall s. f (AD s (Sparse a)) -> g (AD s (Sparse a))) -> f a -> g (f b)
jacobianWith g f = Rank1.jacobianWith g (fmap runAD.f.fmap AD)
{-# INLINE jacobianWith #-}

jacobianWith' :: (Traversable f, Functor g, Num a) => (a -> a -> b) -> (forall s. f (AD s (Sparse a)) -> g (AD s (Sparse a))) -> f a -> g (a, f b)
jacobianWith' g f = Rank1.jacobianWith' g (fmap runAD.f.fmap AD)
{-# INLINE jacobianWith' #-}

grads :: (Traversable f, Num a) => (forall s. f (AD s (Sparse a)) -> AD s (Sparse a)) -> f a -> Cofree f a
grads f = Rank1.grads (runAD.f.fmap AD)
{-# INLINE grads #-}

jacobians :: (Traversable f, Functor g, Num a) => (forall s. f (AD s (Sparse a)) -> g (AD s (Sparse a))) -> f a -> g (Cofree f a)
jacobians f = Rank1.jacobians (fmap runAD.f.fmap AD)
{-# INLINE jacobians #-}

hessian :: (Traversable f, Num a) => (forall s. f (AD s (Sparse a)) -> AD s (Sparse a)) -> f a -> f (f a)
hessian f = Rank1.hessian (runAD.f.fmap AD)
{-# INLINE hessian #-}

hessian' :: (Traversable f, Num a) => (forall s. f (AD s (Sparse a)) -> AD s (Sparse a)) -> f a -> (a, f (a, f a))
hessian' f = Rank1.hessian' (runAD.f.fmap AD)
{-# INLINE hessian' #-}

hessianF :: (Traversable f, Functor g, Num a) => (forall s. f (AD s (Sparse a)) -> g (AD s (Sparse a))) -> f a -> g (f (f a))
hessianF f = Rank1.hessianF (fmap runAD.f.fmap AD)
{-# INLINE hessianF #-}

hessianF' :: (Traversable f, Functor g, Num a) => (forall s. f (AD s (Sparse a)) -> g (AD s (Sparse a))) -> f a -> g (a, f (a, f a))
hessianF' f = Rank1.hessianF' (fmap runAD.f.fmap AD)
{-# INLINE hessianF' #-}
