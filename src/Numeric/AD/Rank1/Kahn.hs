{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2010-2015
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- This module provides reverse-mode Automatic Differentiation using post-hoc linear time
-- topological sorting.
--
-- For reverse mode AD we use 'System.Mem.StableName.StableName' to recover sharing information from
-- the tape to avoid combinatorial explosion, and thus run asymptotically faster
-- than it could without such sharing information, but the use of side-effects
-- contained herein is benign.
--
-----------------------------------------------------------------------------

module Numeric.AD.Rank1.Kahn
  ( Kahn
  , auto
  -- * Gradient
  , grad
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
  -- $vgrad
  , vgrad, vgrad'
  , Grad
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
import Data.Traversable (Traversable)
#endif
import Data.Functor.Compose
import Numeric.AD.Internal.On
import Numeric.AD.Internal.Kahn
import Numeric.AD.Mode

-- $setup
--
-- >>> import Numeric.AD.Internal.Doctest

-- | The 'grad' function calculates the gradient of a non-scalar-to-scalar function with kahn-mode AD in a single pass.
--
-- >>> grad (\[x,y,z] -> x*y+z) [1,2,3]
-- [2,1,1]
grad :: (Traversable f, Num a) => (f (Kahn a) -> Kahn a) -> f a -> f a
grad f as = unbind vs (partialArray bds $ f vs) where
  (vs,bds) = bind as
{-# INLINE grad #-}

-- | The 'grad'' function calculates the result and gradient of a non-scalar-to-scalar function with kahn-mode AD in a single pass.
--
-- >>> grad' (\[x,y,z] -> 4*x*exp y+cos z) [1,2,3]
-- (28.566231899122155,[29.5562243957226,29.5562243957226,-0.1411200080598672])
grad' :: (Traversable f, Num a) => (f (Kahn a) -> Kahn a) -> f a -> (a, f a)
grad' f as = (primal r, unbind vs $ partialArray bds r) where
  (vs, bds) = bind as
  r = f vs
{-# INLINE grad' #-}

-- | @'grad' g f@ function calculates the gradient of a non-scalar-to-scalar function @f@ with kahn-mode AD in a single pass.
-- The gradient is combined element-wise with the argument using the function @g@.
--
-- @
-- 'grad' = 'gradWith' (\_ dx -> dx)
-- 'id' = 'gradWith' const
-- @
--
--
gradWith :: (Traversable f, Num a) => (a -> a -> b) -> (f (Kahn a) -> Kahn a) -> f a -> f b
gradWith g f as = unbindWith g vs (partialArray bds $ f vs) where
  (vs,bds) = bind as
{-# INLINE gradWith #-}

-- | @'grad'' g f@ calculates the result and gradient of a non-scalar-to-scalar function @f@ with kahn-mode AD in a single pass
-- the gradient is combined element-wise with the argument using the function @g@.
--
-- @'grad'' == 'gradWith'' (\_ dx -> dx)@
gradWith' :: (Traversable f, Num a) => (a -> a -> b) -> (f (Kahn a) -> Kahn a) -> f a -> (a, f b)
gradWith' g f as = (primal r, unbindWith g vs $ partialArray bds r) where
  (vs, bds) = bind as
  r = f vs
{-# INLINE gradWith' #-}

-- | The 'jacobian' function calculates the jacobian of a non-scalar-to-non-scalar function with kahn AD lazily in @m@ passes for @m@ outputs.
--
-- >>> jacobian (\[x,y] -> [y,x,x*y]) [2,1]
-- [[0,1],[1,0],[1,2]]
--
-- >>> jacobian (\[x,y] -> [exp y,cos x,x+y]) [1,2]
-- [[0.0,7.38905609893065],[-0.8414709848078965,0.0],[1.0,1.0]]
jacobian :: (Traversable f, Functor g, Num a) => (f (Kahn a) -> g (Kahn a)) -> f a -> g (f a)
jacobian f as = unbind vs . partialArray bds <$> f vs where
  (vs, bds) = bind as
{-# INLINE jacobian #-}

-- | The 'jacobian'' function calculates both the result and the Jacobian of a nonscalar-to-nonscalar function, using @m@ invocations of kahn AD,
-- where @m@ is the output dimensionality. Applying @fmap snd@ to the result will recover the result of 'jacobian'
-- | An alias for 'gradF''
--
-- ghci> jacobian' (\[x,y] -> [y,x,x*y]) [2,1]
-- [(1,[0,1]),(2,[1,0]),(2,[1,2])]
jacobian' :: (Traversable f, Functor g, Num a) => (f (Kahn a) -> g (Kahn a)) -> f a -> g (a, f a)
jacobian' f as = row <$> f vs where
  (vs, bds) = bind as
  row a = (primal a, unbind vs (partialArray bds a))
{-# INLINE jacobian' #-}

-- | 'jacobianWith g f' calculates the Jacobian of a non-scalar-to-non-scalar function @f@ with kahn AD lazily in @m@ passes for @m@ outputs.
--
-- Instead of returning the Jacobian matrix, the elements of the matrix are combined with the input using the @g@.
--
-- @
-- 'jacobian' = 'jacobianWith' (\_ dx -> dx)
-- 'jacobianWith' 'const' = (\f x -> 'const' x '<$>' f x)
-- @
jacobianWith :: (Traversable f, Functor g, Num a) => (a -> a -> b) -> (f (Kahn a) -> g (Kahn a)) -> f a -> g (f b)
jacobianWith g f as = unbindWith g vs . partialArray bds <$> f vs where
  (vs, bds) = bind as
{-# INLINE jacobianWith #-}

-- | 'jacobianWith' g f' calculates both the result and the Jacobian of a nonscalar-to-nonscalar function @f@, using @m@ invocations of kahn AD,
-- where @m@ is the output dimensionality. Applying @fmap snd@ to the result will recover the result of 'jacobianWith'
--
-- Instead of returning the Jacobian matrix, the elements of the matrix are combined with the input using the @g@.
--
-- @'jacobian'' == 'jacobianWith'' (\_ dx -> dx)@
jacobianWith' :: (Traversable f, Functor g, Num a) => (a -> a -> b) -> (f (Kahn a) -> g (Kahn a)) -> f a -> g (a, f b)
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
diff :: Num a => (Kahn a -> Kahn a) -> a -> a
diff f a = derivative $ f (var a 0)
{-# INLINE diff #-}

-- | The 'diff'' function calculates the value and derivative, as a
-- pair, of a scalar-to-scalar function.
--
--
-- >>> diff' sin 0
-- (0.0,1.0)
diff' :: Num a => (Kahn a -> Kahn a) -> a -> (a, a)
diff' f a = derivative' $ f (var a 0)
{-# INLINE diff' #-}

-- | Compute the derivatives of a function that returns a vector with regards to its single input.
--
-- >>> diffF (\a -> [sin a, cos a]) 0
-- [1.0,0.0]
diffF :: (Functor f, Num a) => (Kahn a -> f (Kahn a)) -> a -> f a
diffF f a = derivative <$> f (var a 0)
{-# INLINE diffF #-}

-- | Compute the derivatives of a function that returns a vector with regards to its single input
-- as well as the primal answer.
--
-- >>> diffF' (\a -> [sin a, cos a]) 0
-- [(0.0,1.0),(1.0,0.0)]
diffF' :: (Functor f, Num a) => (Kahn a -> f (Kahn a)) -> a -> f (a, a)
diffF' f a = derivative' <$> f (var a 0)
{-# INLINE diffF' #-}


-- | Compute the 'hessian' via the 'jacobian' of the gradient. gradient is computed in 'Kahn' mode and then the 'jacobian' is computed in 'Kahn' mode.
--
-- However, since the @'grad' f :: f a -> f a@ is square this is not as fast as using the forward-mode 'jacobian' of a reverse mode gradient provided by 'Numeric.AD.hessian'.
--
-- >>> hessian (\[x,y] -> x*y) [1,2]
-- [[0,1],[1,0]]
hessian :: (Traversable f, Num a) => (f (On (Kahn (Kahn a))) -> (On (Kahn (Kahn a)))) -> f a -> f (f a)
hessian f = jacobian (grad (off . f . fmap On))

-- | Compute the order 3 Hessian tensor on a non-scalar-to-non-scalar function via the 'Kahn'-mode Jacobian of the 'Kahn'-mode Jacobian of the function.
--
-- Less efficient than 'Numeric.AD.Mode.Mixed.hessianF'.
--
-- >>> hessianF (\[x,y] -> [x*y,x+y,exp x*cos y]) [1,2 :: RDouble]
-- [[[0.0,1.0],[1.0,0.0]],[[0.0,0.0],[0.0,0.0]],[[-1.131204383757,-2.471726672005],[-2.471726672005,1.131204383757]]]
hessianF :: (Traversable f, Functor g, Num a) => (f (On (Kahn (Kahn a))) -> g (On (Kahn (Kahn a)))) -> f a -> g (f (f a))
hessianF f = getCompose . jacobian (Compose . jacobian (fmap off . f . fmap On))

-- $vgrad
--
-- Variadic combinators for variadic mixed-mode automatic differentiation.
--
-- Unfortunately, variadicity comes at the expense of being able to use
-- quantification to avoid sensitivity confusion, so be careful when
-- counting the number of 'auto' calls you use when taking the gradient
-- of a function that takes gradients!
