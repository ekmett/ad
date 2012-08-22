{-# LANGUAGE Rank2Types, TemplateHaskell, BangPatterns, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Mode.Reverse
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

module Numeric.AD.Mode.Reverse
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
    , hessianF
    -- * Derivatives
    , diff
    , diff'
    , diffF
    , diffF'
    -- * Unsafe Variadic Gradient
    , vgrad, vgrad'
    , Grad
    ) where

import Control.Applicative ((<$>))
import Data.Traversable (Traversable)

import Numeric.AD.Types
import Numeric.AD.Internal.Classes
import Numeric.AD.Internal.Composition
import Numeric.AD.Internal.Reverse
import Numeric.AD.Internal.Var

-- | The 'grad' function calculates the gradient of a non-scalar-to-scalar function with 'Reverse' AD in a single pass.
--
-- >>> grad (\[x,y,z] -> x*y+z) [1,2,3]
-- [2,1,1]

grad :: (Traversable f, Num a) => (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> f a
grad f as = unbind vs (partialArray bds $ f vs)
    where (vs,bds) = bind as
{-# INLINE grad #-}

-- | The 'grad'' function calculates the result and gradient of a non-scalar-to-scalar function with 'Reverse' AD in a single pass.
--
-- >>> grad' (\[x,y,z] -> 4*x*exp y+cos z) [1,2,3]
-- (28.566231899122155,[29.5562243957226,29.5562243957226,-0.1411200080598672])
grad' :: (Traversable f, Num a) => (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> (a, f a)
grad' f as = (primal r, unbind vs $ partialArray bds r)
    where (vs, bds) = bind as
          r = f vs
{-# INLINE grad' #-}

-- | @'grad' g f@ function calculates the gradient of a non-scalar-to-scalar function @f@ with reverse-mode AD in a single pass.
-- The gradient is combined element-wise with the argument using the function @g@.
--
-- @
-- 'grad' = 'gradWith' (\_ dx -> dx)
-- 'id' = 'gradWith' const
-- @
--
--
gradWith :: (Traversable f, Num a) => (a -> a -> b) -> (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> f b
gradWith g f as = unbindWith g vs (partialArray bds $ f vs)
    where (vs,bds) = bind as
{-# INLINE gradWith #-}

-- | @'grad'' g f@ calculates the result and gradient of a non-scalar-to-scalar function @f@ with 'Reverse' AD in a single pass
-- the gradient is combined element-wise with the argument using the function @g@.
--
-- @'grad'' == 'gradWith'' (\_ dx -> dx)@
gradWith' :: (Traversable f, Num a) => (a -> a -> b) -> (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> (a, f b)
gradWith' g f as = (primal r, unbindWith g vs $ partialArray bds r)
    where (vs, bds) = bind as
          r = f vs
{-# INLINE gradWith' #-}

-- | The 'jacobian' function calculates the jacobian of a non-scalar-to-non-scalar function with reverse AD lazily in @m@ passes for @m@ outputs.
--
-- >>> jacobian (\[x,y] -> [y,x,x*y]) [2,1]
-- [[0,1],[1,0],[1,2]]
--
-- >>> jacobian (\[x,y] -> [exp y,cos x,x+y]) [1,2]
-- [[0.0,7.38905609893065],[-0.8414709848078965,0.0],[1.0,1.0]]
jacobian :: (Traversable f, Functor g, Num a) => (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (f a)
jacobian f as = unbind vs . partialArray bds <$> f vs where
    (vs, bds) = bind as
{-# INLINE jacobian #-}

-- | The 'jacobian'' function calculates both the result and the Jacobian of a nonscalar-to-nonscalar function, using @m@ invocations of reverse AD,
-- where @m@ is the output dimensionality. Applying @fmap snd@ to the result will recover the result of 'jacobian'
-- | An alias for 'gradF''
--
-- ghci> jacobian' (\[x,y] -> [y,x,x*y]) [2,1]
-- [(1,[0,1]),(2,[1,0]),(2,[1,2])]
jacobian' :: (Traversable f, Functor g, Num a) => (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (a, f a)
jacobian' f as = row <$> f vs where
    (vs, bds) = bind as
    row a = (primal a, unbind vs (partialArray bds a))
{-# INLINE jacobian' #-}

-- | 'jacobianWith g f' calculates the Jacobian of a non-scalar-to-non-scalar function @f@ with reverse AD lazily in @m@ passes for @m@ outputs.
--
-- Instead of returning the Jacobian matrix, the elements of the matrix are combined with the input using the @g@.
--
-- @
-- 'jacobian' = 'jacobianWith' (\_ dx -> dx)
-- 'jacobianWith' 'const' = (\f x -> 'const' x '<$>' f x)
-- @
jacobianWith :: (Traversable f, Functor g, Num a) => (a -> a -> b) -> (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (f b)
jacobianWith g f as = unbindWith g vs . partialArray bds <$> f vs where
    (vs, bds) = bind as
{-# INLINE jacobianWith #-}

-- | 'jacobianWith' g f' calculates both the result and the Jacobian of a nonscalar-to-nonscalar function @f@, using @m@ invocations of reverse AD,
-- where @m@ is the output dimensionality. Applying @fmap snd@ to the result will recover the result of 'jacobianWith'
--
-- Instead of returning the Jacobian matrix, the elements of the matrix are combined with the input using the @g@.
--
-- @'jacobian'' == 'jacobianWith'' (\_ dx -> dx)@
jacobianWith' :: (Traversable f, Functor g, Num a) => (a -> a -> b) -> (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (a, f b)
jacobianWith' g f as = row <$> f vs where
    (vs, bds) = bind as
    row a = (primal a, unbindWith g vs (partialArray bds a))
{-# INLINE jacobianWith' #-}

-- | Compute the derivative of a function.
--
-- >>> diff sin 0
-- 1.0
--
-- >>> cos 0
-- 1.0
diff :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> a
diff f a = derivative $ f (var a 0)
{-# INLINE diff #-}

-- | The 'diff'' function calculates the value and derivative, as a
-- pair, of a scalar-to-scalar function.
--
--
-- >>> diff' sin 0
-- (0.0,1.0)
diff' :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> (a, a)
diff' f a = derivative' $ f (var a 0)
{-# INLINE diff' #-}

-- | Compute the derivatives of a function that returns a vector with regards to its single input.
--
-- >>> diffF (\a -> [sin a, cos a]) 0
-- [1.0,0.0]
diffF :: (Functor f, Num a) => (forall s. Mode s => AD s a -> f (AD s a)) -> a -> f a
diffF f a = derivative <$> f (var a 0)
{-# INLINE diffF #-}

-- | Compute the derivatives of a function that returns a vector with regards to its single input
-- as well as the primal answer.
--
-- >>> diffF' (\a -> [sin a, cos a]) 0
-- [(0.0,1.0),(1.0,0.0)]
diffF' :: (Functor f, Num a) => (forall s. Mode s => AD s a -> f (AD s a)) -> a -> f (a, a)
diffF' f a = derivative' <$> f (var a 0)
{-# INLINE diffF' #-}

-- | Compute the 'hessian' via the 'jacobian' of the gradient. gradient is computed in reverse mode and then the 'jacobian' is computed in reverse mode.
--
-- However, since the @'grad' f :: f a -> f a@ is square this is not as fast as using the forward-mode 'jacobian' of a reverse mode gradient provided by 'Numeric.AD.hessian'.
--
-- >>> hessian (\[x,y] -> x*y) [1,2]
-- [[0,1],[1,0]]
hessian :: (Traversable f, Num a) => (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> f (f a)
hessian f = jacobian (grad (decomposeMode . f . fmap composeMode))

-- | Compute the order 3 Hessian tensor on a non-scalar-to-non-scalar function via the reverse-mode Jacobian of the reverse-mode Jacobian of the function.
--
-- Less efficient than 'Numeric.AD.Mode.Mixed.hessianF'.
--
-- >>> hessianF (\[x,y] -> [x*y,x+y,exp x*cos y]) [1,2]
-- [[[0.0,1.0],[1.0,0.0]],[[0.0,0.0],[0.0,0.0]],[[-1.1312043837568135,-2.4717266720048188],[-2.4717266720048188,1.1312043837568135]]]
hessianF :: (Traversable f, Functor g, Num a) => (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (f (f a))
hessianF f = decomposeFunctor . jacobian (ComposeFunctor . jacobian (fmap decomposeMode . f . fmap composeMode))

