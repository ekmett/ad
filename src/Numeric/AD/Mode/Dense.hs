{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   : (c) Edward Kmett 2010-2015
-- License     : BSD3
-- Maintainer  : ekmett@gmail.com
-- Stability   : experimental
-- Portability : GHC only
--
-- First order dense forward mode using 'Traversable' functors
--
-----------------------------------------------------------------------------

module Numeric.AD.Mode.Dense
  ( AD, Dense, auto
  -- * Dense Gradients
  , grad
  , grad'
  , gradWith
  , gradWith'

  -- * Dense Jacobians (synonyms)
  , jacobian
  , jacobian'
  , jacobianWith
  , jacobianWith'
  ) where

#if __GLASGOW_HASKELL__ < 710
import Data.Traversable (Traversable)
#endif
import Numeric.AD.Internal.Dense (Dense)
import qualified Numeric.AD.Rank1.Dense as Rank1
import Numeric.AD.Internal.Type
import Numeric.AD.Mode

-- | The 'grad' function calculates the gradient of a non-scalar-to-scalar function with dense-mode AD in a single pass.
--
-- >>> grad (\[x,y,z] -> x*y+z) [1,2,3]
-- [2,1,1]
grad :: (Traversable f, Num a) => (forall s. f (AD s (Dense f a)) -> AD s (Dense f a)) -> f a -> f a
grad f = Rank1.grad (runAD.f.fmap AD)
{-# INLINE grad #-}

grad' :: (Traversable f, Num a) => (forall s. f (AD s (Dense f a)) -> AD s (Dense f a)) -> f a -> (a, f a)
grad' f = Rank1.grad' (runAD.f.fmap AD)
{-# INLINE grad' #-}

gradWith :: (Traversable f, Num a) => (a -> a -> b) -> (forall s. f (AD s (Dense f a)) -> AD s (Dense f a)) -> f a -> f b
gradWith g f = Rank1.gradWith g (runAD.f.fmap AD)
{-# INLINE gradWith #-}

gradWith' :: (Traversable f, Num a) => (a -> a -> b) -> (forall s. f (AD s (Dense f a)) -> AD s (Dense f a)) -> f a -> (a, f b)
gradWith' g f = Rank1.gradWith' g (runAD.f.fmap AD)
{-# INLINE gradWith' #-}

jacobian :: (Traversable f, Functor g, Num a) => (forall s. f (AD s (Dense f a)) -> g (AD s (Dense f a))) -> f a -> g (f a)
jacobian f = Rank1.jacobian (fmap runAD.f.fmap AD)
{-# INLINE jacobian #-}

jacobian' :: (Traversable f, Functor g, Num a) => (forall s. f (AD s (Dense f a)) -> g (AD s (Dense f a))) -> f a -> g (a, f a)
jacobian' f = Rank1.jacobian' (fmap runAD.f.fmap AD)
{-# INLINE jacobian' #-}

jacobianWith :: (Traversable f, Functor g, Num a) => (a -> a -> b) -> (forall s. f (AD s (Dense f a)) -> g (AD s (Dense f a))) -> f a -> g (f b)
jacobianWith g f = Rank1.jacobianWith g (fmap runAD.f.fmap AD)
{-# INLINE jacobianWith #-}

jacobianWith' :: (Traversable f, Functor g, Num a) => (a -> a -> b) -> (forall s. f (AD s (Dense f a)) -> g (AD s (Dense f a))) -> f a -> g (a, f b)
jacobianWith' g f = Rank1.jacobianWith' g (fmap runAD.f.fmap AD)
{-# INLINE jacobianWith' #-}
