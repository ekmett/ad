{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   : (c) Edward Kmett 2010-2021
-- License     : BSD3
-- Maintainer  : ekmett@gmail.com
-- Stability   : experimental
-- Portability : GHC only
--
-----------------------------------------------------------------------------

module Numeric.AD.Rank1.Dense
  ( Dense
  , auto
  -- * Sparse Gradients
  , grad
  , grad'
  , gradWith
  , gradWith'

  -- * Sparse Jacobians (synonyms)
  , jacobian
  , jacobian'
  , jacobianWith
  , jacobianWith'

  ) where

#if __GLASGOW_HASKELL__ < 710
import Data.Traversable
#endif
import Numeric.AD.Internal.Dense
import Numeric.AD.Internal.Combinators
import Numeric.AD.Mode

second :: (a -> b) -> (c, a) -> (c, b)
second g (a,b) = (a, g b)
{-# INLINE second #-}

grad :: (Traversable f, Num a) => (f (Dense f a) -> Dense f a) -> f a -> f a
grad f as = ds (0 <$ as) $ apply f as
{-# INLINE grad #-}

grad' :: (Traversable f, Num a) => (f (Dense f a) -> Dense f a) -> f a -> (a, f a)
grad' f as = ds' (0 <$ as) $ apply f as
{-# INLINE grad' #-}

gradWith :: (Traversable f, Num a) => (a -> a -> b) -> (f (Dense f a) -> Dense f a) -> f a -> f b
gradWith g f as = zipWithT g as $ grad f as
{-# INLINE gradWith #-}

gradWith' :: (Traversable f, Num a) => (a -> a -> b) -> (f (Dense f a) -> Dense f a) -> f a -> (a, f b)
gradWith' g f as = second (zipWithT g as) $ grad' f as
{-# INLINE gradWith' #-}

jacobian :: (Traversable f, Functor g, Num a) => (f (Dense f a) -> g (Dense f a)) -> f a -> g (f a)
jacobian f as = ds (0 <$ as) <$> apply f as
{-# INLINE jacobian #-}

jacobian' :: (Traversable f, Functor g, Num a) => (f (Dense f a) -> g (Dense f a)) -> f a -> g (a, f a)
jacobian' f as = ds' (0 <$ as) <$> apply f as
{-# INLINE jacobian' #-}

jacobianWith :: (Traversable f, Functor g, Num a) => (a -> a -> b) -> (f (Dense f a) -> g (Dense f a)) -> f a -> g (f b)
jacobianWith g f as = zipWithT g as <$> jacobian f as
{-# INLINE jacobianWith #-}

jacobianWith' :: (Traversable f, Functor g, Num a) => (a -> a -> b) -> (f (Dense f a) -> g (Dense f a)) -> f a -> g (a, f b)
jacobianWith' g f as = second (zipWithT g as) <$> jacobian' f as
{-# INLINE jacobianWith' #-}
