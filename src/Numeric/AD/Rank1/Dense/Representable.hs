{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   : (c) Edward Kmett 2010-2021
-- License     : BSD3
-- Maintainer  : ekmett@gmail.com
-- Stability   : experimental
-- Portability : GHC only
--
-- Dense forward mode automatic differentiation with representable functors.
--
-----------------------------------------------------------------------------

module Numeric.AD.Rank1.Dense.Representable
  ( Repr
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
import Data.Functor
#endif
import Data.Functor.Rep
import Numeric.AD.Internal.Dense.Representable
import Numeric.AD.Mode

second :: (a -> b) -> (c, a) -> (c, b)
second g (a,b) = (a, g b)
{-# INLINE second #-}

grad :: (Representable f, Eq (Rep f), Num a) => (f (Repr f a) -> Repr f a) -> f a -> f a
grad f as = ds (pureRep 0) $ apply f as
{-# INLINE grad #-}

grad' :: (Representable f, Eq (Rep f), Num a) => (f (Repr f a) -> Repr f a) -> f a -> (a, f a)
grad' f as = ds' (pureRep 0) $ apply f as
{-# INLINE grad' #-}

gradWith :: (Representable f, Eq (Rep f), Num a) => (a -> a -> b) -> (f (Repr f a) -> Repr f a) -> f a -> f b
gradWith g f as = liftR2 g as $ grad f as
{-# INLINE gradWith #-}

gradWith' :: (Representable f, Eq (Rep f), Num a) => (a -> a -> b) -> (f (Repr f a) -> Repr f a) -> f a -> (a, f b)
gradWith' g f as = second (liftR2 g as) $ grad' f as
{-# INLINE gradWith' #-}

jacobian :: (Representable f, Eq (Rep f), Functor g, Num a) => (f (Repr f a) -> g (Repr f a)) -> f a -> g (f a)
jacobian f as = ds (0 <$ as) <$> apply f as
{-# INLINE jacobian #-}

jacobian' :: (Representable f, Eq (Rep f), Functor g, Num a) => (f (Repr f a) -> g (Repr f a)) -> f a -> g (a, f a)
jacobian' f as = ds' (0 <$ as) <$> apply f as
{-# INLINE jacobian' #-}

jacobianWith :: (Representable f, Eq (Rep f), Functor g, Num a) => (a -> a -> b) -> (f (Repr f a) -> g (Repr f a)) -> f a -> g (f b)
jacobianWith g f as = liftR2 g as <$> jacobian f as
{-# INLINE jacobianWith #-}

jacobianWith' :: (Representable f, Eq (Rep f), Functor g, Num a) => (a -> a -> b) -> (f (Repr f a) -> g (Repr f a)) -> f a -> g (a, f b)
jacobianWith' g f as = second (liftR2 g as) <$> jacobian' f as
{-# INLINE jacobianWith' #-}
