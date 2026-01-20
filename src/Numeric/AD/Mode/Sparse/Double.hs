{-# LANGUAGE Rank2Types #-}
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

module Numeric.AD.Mode.Sparse.Double
  ( AD, SparseDouble, auto
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
import Numeric.AD.Internal.Sparse.Double (SparseDouble)
import qualified Numeric.AD.Rank1.Sparse.Double as Rank1
import Numeric.AD.Internal.Type
import Numeric.AD.Mode

-- | The 'grad' function calculates the gradient of a non-scalar-to-scalar function with sparse-mode AD in a single pass.
--
--
-- >>> grad (\[x,y,z] -> x*y+z) [1,2,3]
-- [2.0,1.0,1.0]
--
-- >>> grad (\[x,y] -> x**y) [0,2]
-- [0.0,NaN]
grad
  :: Traversable f
  => (forall s. f (AD s SparseDouble) -> AD s SparseDouble)
  -> f Double
  -> f Double
grad f = Rank1.grad (runAD.f.fmap AD)
{-# INLINE grad #-}

grad'
  :: Traversable f
  => (forall s. f (AD s SparseDouble) -> AD s SparseDouble)
  -> f Double
  -> (Double, f Double)
grad' f = Rank1.grad' (runAD.f.fmap AD)
{-# INLINE grad' #-}

gradWith
  :: Traversable f
  => (Double -> Double -> b)
  -> (forall s. f (AD s SparseDouble) -> AD s SparseDouble)
  -> f Double
  -> f b
gradWith g f = Rank1.gradWith g (runAD.f.fmap AD)
{-# INLINE gradWith #-}

gradWith'
  :: Traversable f
  => (Double -> Double -> b)
  -> (forall s. f (AD s SparseDouble) -> AD s SparseDouble)
  -> f Double
  -> (Double, f b)
gradWith' g f = Rank1.gradWith' g (runAD.f.fmap AD)
{-# INLINE gradWith' #-}

jacobian
  :: (Traversable f, Functor g)
  => (forall s. f (AD s SparseDouble) -> g (AD s SparseDouble))
  -> f Double -> g (f Double)
jacobian f = Rank1.jacobian (fmap runAD.f.fmap AD)
{-# INLINE jacobian #-}

jacobian'
  :: (Traversable f, Functor g)
  => (forall s. f (AD s SparseDouble) -> g (AD s SparseDouble))
  -> f Double
  -> g (Double, f Double)
jacobian' f = Rank1.jacobian' (fmap runAD.f.fmap AD)
{-# INLINE jacobian' #-}

jacobianWith
  :: (Traversable f, Functor g)
  => (Double -> Double -> b)
  -> (forall s. f (AD s SparseDouble) -> g (AD s SparseDouble))
  -> f Double
  -> g (f b)
jacobianWith g f = Rank1.jacobianWith g (fmap runAD.f.fmap AD)
{-# INLINE jacobianWith #-}

jacobianWith'
  :: (Traversable f, Functor g)
  => (Double -> Double -> b)
  -> (forall s. f (AD s SparseDouble) -> g (AD s SparseDouble))
  -> f Double
  -> g (Double, f b)
jacobianWith' g f = Rank1.jacobianWith' g (fmap runAD.f.fmap AD)
{-# INLINE jacobianWith' #-}

grads
  :: Traversable f
  => (forall s. f (AD s SparseDouble) -> AD s SparseDouble)
  -> f Double -> Cofree f Double
grads f = Rank1.grads (runAD.f.fmap AD)
{-# INLINE grads #-}

jacobians
  :: (Traversable f, Functor g)
  => (forall s. f (AD s SparseDouble) -> g (AD s SparseDouble))
  -> f Double
  -> g (Cofree f Double)
jacobians f = Rank1.jacobians (fmap runAD.f.fmap AD)
{-# INLINE jacobians #-}

hessian
  :: Traversable f
  => (forall s. f (AD s SparseDouble) -> AD s SparseDouble)
  -> f Double
  -> f (f Double)
hessian f = Rank1.hessian (runAD.f.fmap AD)
{-# INLINE hessian #-}

hessian'
  :: Traversable f
  => (forall s. f (AD s SparseDouble) -> AD s SparseDouble)
  -> f Double -> (Double, f (Double, f Double))
hessian' f = Rank1.hessian' (runAD.f.fmap AD)
{-# INLINE hessian' #-}

hessianF
  :: (Traversable f, Functor g)
  => (forall s. f (AD s SparseDouble) -> g (AD s SparseDouble))
  -> f Double -> g (f (f Double))
hessianF f = Rank1.hessianF (fmap runAD.f.fmap AD)
{-# INLINE hessianF #-}

hessianF'
  :: (Traversable f, Functor g)
  => (forall s. f (AD s SparseDouble) -> g (AD s SparseDouble))
  -> f Double
  -> g (Double, f (Double, f Double))
hessianF' f = Rank1.hessianF' (fmap runAD.f.fmap AD)
{-# INLINE hessianF' #-}
