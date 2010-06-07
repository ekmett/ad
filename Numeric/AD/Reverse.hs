{-# LANGUAGE Rank2Types, TemplateHaskell, BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Reverse
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Mixed-Mode Automatic Differentiation.
--
-- For reverse mode AD we use 'System.Mem.StableName.StableName' to recover sharing information from
-- the tape to avoid combinatorial explosion, and thus run asymptotically faster
-- than it could without such sharing information, but the use of side-effects
-- contained herein is benign.
--
-----------------------------------------------------------------------------

module Numeric.AD.Reverse
    (
    -- * Gradient
      grad
    , grad'
    , gradWith
    , gradWith'
    -- * Jacobian
    , jacobian
    , jacobian'
    , jacobianWith
    , jacobianWith'
    -- * Hessian
    , hessian
    , hessianM
    , hessianTensor
    
    -- * Derivatives
    , diff
    , diff'
    , diffF
    , diffF'
    -- * Monadic Combinators
    , diffM
    , diffM'
    , gradM
    , gradM'
    , gradWithM
    , gradWithM'
    -- * Synonyms
    , gradF
    , gradF'
    , gradWithF
    , gradWithF'
    -- * Exposed Types
    , AD(..)
    , Mode(..)
    ) where

import Control.Monad (liftM)
import Control.Applicative (WrappedMonad(..),(<$>))
import Data.Traversable (Traversable)

import Numeric.AD.Internal
import Numeric.AD.Internal.Composition
import Numeric.AD.Internal.Reverse

-- | The 'grad' function calculates the gradient of a non-scalar-to-scalar function with 'Reverse' AD in a single pass.
grad :: (Traversable f, Num a) => (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> f a
grad f as = unbind vs (partialArray bds $ f vs)
    where (vs,bds) = bind as
{-# INLINE grad #-}

-- | The 'grad'' function calculates the result and gradient of a non-scalar-to-scalar function with 'Reverse' AD in a single pass.
grad' :: (Traversable f, Num a) => (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> (a, f a)
grad' f as = (primal r, unbind vs $ partialArray bds r)
    where (vs, bds) = bind as
          r = f vs
{-# INLINE grad' #-}

-- | @'grad' g f@ function calculates the gradient of a non-scalar-to-scalar function @f@ with reverse-mode AD in a single pass.
-- The gradient is combined element-wise with the argument using the function @g@.
--
-- > grad == gradWith (\_ dx -> dx)
-- > id == gradWith const
gradWith :: (Traversable f, Num a) => (a -> a -> b) -> (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> f b
gradWith g f as = unbindWith g vs (partialArray bds $ f vs)
    where (vs,bds) = bind as
{-# INLINE gradWith #-}

-- | @'grad'' g f@ calculates the result and gradient of a non-scalar-to-scalar function @f@ with 'Reverse' AD in a single pass
-- the gradient is combined element-wise with the argument using the function @g@.
--
-- > grad' == gradWith' (\_ dx -> dx)
gradWith' :: (Traversable f, Num a) => (a -> a -> b) -> (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> (a, f b)
gradWith' g f as = (primal r, unbindWith g vs $ partialArray bds r)
    where (vs, bds) = bind as
          r = f vs
{-# INLINE gradWith' #-}

-- | The 'gradF' function calculates the jacobian of a non-scalar-to-non-scalar function with reverse AD lazily in @m@ passes for @m@ outputs.
gradF :: (Traversable f, Functor g, Num a) => (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (f a)
gradF = jacobian
{-# INLINE gradF #-}

-- | An alias for 'gradF'
jacobian :: (Traversable f, Functor g, Num a) => (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (f a)
jacobian f as = unbind vs . partialArray bds <$> f vs where
    (vs, bds) = bind as
{-# INLINE jacobian #-}

-- | The 'gradF'' function calculates both the result and the Jacobian of a nonscalar-to-nonscalar function, using @m@ invocations of reverse AD,
-- where @m@ is the output dimensionality. Applying @fmap snd@ to the result will recover the result of 'gradF'
gradF' :: (Traversable f, Functor g, Num a) => (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (a, f a)
gradF' = jacobian' 
{-# INLINE gradF' #-}

-- | An alias for 'gradF''
jacobian' :: (Traversable f, Functor g, Num a) => (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (a, f a)
jacobian' f as = row <$> f vs where
    (vs, bds) = bind as
    row a = (primal a, unbind vs (partialArray bds a))
{-# INLINE jacobian' #-}

-- | 'gradWithF g f' calculates the Jacobian of a non-scalar-to-non-scalar function @f@ with reverse AD lazily in @m@ passes for @m@ outputs.
--
-- Instead of returning the Jacobian matrix, the elements of the matrix are combined with the input using the @g@.
--
-- > gradF == gradWithF (\_ dx -> dx)
-- > gradWithF const == (\f x -> const x <$> f x)
--
gradWithF :: (Traversable f, Functor g, Num a) => (a -> a -> b) -> (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (f b)
gradWithF g f as = unbindWith g vs . partialArray bds <$> f vs where
    (vs, bds) = bind as
{-# INLINE gradWithF #-}

-- | An alias for 'gradWithF'.
jacobianWith :: (Traversable f, Functor g, Num a) => (a -> a -> b) -> (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (f b)
jacobianWith = gradWithF 
{-# INLINE jacobianWith #-}

-- | 'gradWithF' g f' calculates both the result and the Jacobian of a nonscalar-to-nonscalar function @f@, using @m@ invocations of reverse AD,
-- where @m@ is the output dimensionality. Applying @fmap snd@ to the result will recover the result of 'gradWithF'
--
-- Instead of returning the Jacobian matrix, the elements of the matrix are combined with the input using the @g@.
--
-- > jacobian' == gradWithF' (\_ dx -> dx)
--
gradWithF' :: (Traversable f, Functor g, Num a) => (a -> a -> b) -> (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (a, f b)
gradWithF' g f as = row <$> f vs where
    (vs, bds) = bind as
    row a = (primal a, unbindWith g vs (partialArray bds a))
{-# INLINE gradWithF' #-}

-- | An alias for 'gradWithF''
jacobianWith' :: (Traversable f, Functor g, Num a) => (a -> a -> b) -> (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (a, f b)
jacobianWith' = gradWithF'
{-# INLINE jacobianWith' #-}

diff :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> a
diff f a = derivative $ f (var a 0)
{-# INLINE diff #-}

-- | The 'd'' function calculates the value and derivative, as a
-- pair, of a scalar-to-scalar function.
diff' :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> (a, a)
diff' f a = derivative' $ f (var a 0)
{-# INLINE diff' #-}

diffF :: (Functor f, Num a) => (forall s. Mode s => AD s a -> f (AD s a)) -> a -> f a
diffF f a = derivative <$> f (var a 0)
{-# INLINE diffF #-}

diffF' :: (Functor f, Num a) => (forall s. Mode s => AD s a -> f (AD s a)) -> a -> f (a, a)
diffF' f a = derivative' <$> f (var a 0)
{-# INLINE diffF' #-}

-- * Monadic Combinators

diffM :: (Monad m, Num a) => (forall s. Mode s => AD s a -> m (AD s a)) -> a -> m a
diffM f a = liftM derivative $ f (var a 0)
{-# INLINE diffM #-}

diffM' :: (Monad m, Num a) => (forall s. Mode s => AD s a -> m (AD s a)) -> a -> m (a, a)
diffM' f a = liftM derivative' $ f (var a 0)
{-# INLINE diffM' #-}

gradM :: (Traversable f, Monad m, Num a) => (forall s. Mode s => f (AD s a) -> m (AD s a)) -> f a -> m (f a)
gradM f = unwrapMonad . jacobian (WrapMonad . f)
{-# INLINE gradM #-}

gradM' :: (Traversable f, Monad m, Num a) => (forall s. Mode s => f (AD s a) -> m (AD s a)) -> f a -> m (a, f a)
gradM' f = unwrapMonad . jacobian' (WrapMonad . f)
{-# INLINE gradM' #-}

gradWithM :: (Traversable f, Monad m, Num a) => (a -> a -> b) -> (forall s. Mode s => f (AD s a) -> m (AD s a)) -> f a -> m (f b)
gradWithM g f = unwrapMonad . jacobianWith g (WrapMonad . f)

gradWithM' :: (Traversable f, Monad m, Num a) => (a -> a -> b) -> (forall s. Mode s => f (AD s a) -> m (AD s a)) -> f a -> m (a, f b)
gradWithM' g f = unwrapMonad . jacobianWith' g (WrapMonad . f)

-- | Compute the hessian via the jacobian of the gradient. gradient is computed in reverse mode and then the jacobian is computed in reverse mode.
--
-- However, since the @'grad f :: f a -> f a'@ is square this is not as fast as using the forward-mode Jacobian of a reverse mode gradient provided by 'Numeric.AD.hessian' in "Numeric.AD".
hessian :: (Traversable f, Num a) => (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> f (f a)
hessian f = jacobian (grad (decomposeMode . f . fmap composeMode))

-- | Compute the order 3 Hessian tensor on a non-scalar-to-non-scalar function via the forward-mode Jacobian of the mixed-mode Jacobian of the function.
--
-- While this is less efficient than 'Numeric.AD.hessianTensor' from "Numeric.AD" or 'Numeric.AD.Forward.hessianTensor' from "Numeric.AD.Forward", the type signature is more permissive with regards to the output non-scalar, and it may be more efficient if only a few coefficients of the result are consumed.
hessianTensor :: (Traversable f, Functor g, Num a) => (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (f (f a))
hessianTensor f = decomposeFunctor . jacobian (ComposeFunctor . jacobian (fmap decomposeMode . f . fmap composeMode))

-- | Compute the hessian via the reverse-mode jacobian of the reverse-mode gradient of a non-scalar-to-scalar monadic action. 
--
-- While this is less efficient than 'Numeric.AD.hessianTensor' from "Numeric.AD" or 'Numeric.AD.Forward.hessianTensor' from "Numeric.AD.Forward", the type signature is more permissive with regards to the output non-scalar, and it may be more efficient if only a few coefficients of the result are consumed.
hessianM :: (Traversable f, Monad m, Num a) => (forall s. Mode s => f (AD s a) -> m (AD s a)) -> f a -> m (f (f a))
hessianM f = unwrapMonad . hessianTensor (WrapMonad . f)
