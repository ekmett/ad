{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2010-2015
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Reverse-mode automatic differentiation using Wengert lists and
-- Data.Reflection
--
-- This version is specialized to `Double` enabling the entire
-- structure to be unboxed.
--
-----------------------------------------------------------------------------

module Numeric.AD.Mode.Reverse.Double
  ( ReverseDouble, auto
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
  ) where

#if __GLASGOW_HASKELL__ < 710
import Data.Functor ((<$>))
import Data.Traversable (Traversable)
#endif
import Data.Functor.Compose
import Data.Reflection (Reifies)
import Numeric.AD.Internal.On
import qualified Numeric.AD.Internal.Reverse as R
import qualified Numeric.AD.Mode.Reverse as M
import Numeric.AD.Internal.Reverse.Double
import Numeric.AD.Mode

-- $setup
--
-- >>> import Numeric.AD.Internal.Doctest

-- | The 'grad' function calculates the gradient of a non-scalar-to-scalar function with reverse-mode AD in a single pass.
--
--
-- >>> grad (\[x,y,z] -> x*y+z) [1,2,3]
-- [2.0,1.0,1.0]
--
-- >>> grad (\[x,y] -> x**y) [0,2]
-- [0.0,NaN]
grad :: (Traversable f) => (forall s. Reifies s Tape => f (ReverseDouble s) -> ReverseDouble s) -> f Double -> f Double
grad f as = reifyTape (snd bds) $ \p -> unbind vs $! partialArrayOf p bds $! f vs where
  (vs, bds) = bind as
{-# INLINE grad #-}

-- | The 'grad'' function calculates the result and gradient of a non-scalar-to-scalar function with reverse-mode AD ƒin a single pass.
--
-- >>> grad' (\[x,y,z] -> x*y+z) [1,2,3]
-- (5.0,[2.0,1.0,1.0])
grad' :: (Traversable f) => (forall s. Reifies s Tape => f (ReverseDouble s) -> ReverseDouble s) -> f Double -> (Double, f Double)
grad' f as = reifyTape (snd bds) $ \p -> case f vs of
   r -> (primal r, unbind vs $! partialArrayOf p bds $! r)
  where (vs, bds) = bind as
{-# INLINE grad' #-}

-- | @'grad' g f@ function calculates the gradient of a non-scalar-to-scalar function @f@ with reverse-mode AD in a single pass.
-- The gradient is combined element-wise with the argument using the function @g@.
--
-- @
-- 'grad' == 'gradWith' (\_ dx -> dx)
-- 'id' == 'gradWith' 'const'
-- @
gradWith :: (Traversable f) => (Double -> Double -> b) -> (forall s. Reifies s Tape => f (ReverseDouble s) -> ReverseDouble s) -> f Double -> f b
gradWith g f as = reifyTape (snd bds) $ \p -> unbindWith g vs $! partialArrayOf p bds $! f vs
  where (vs,bds) = bind as
{-# INLINE gradWith #-}

-- | @'grad'' g f@ calculates the result and gradient of a non-scalar-to-scalar function @f@ with reverse-mode AD in a single pass
-- the gradient is combined element-wise with the argument using the function @g@.
--
-- @
-- 'grad'' == 'gradWith'' (\_ dx -> dx)
-- @
gradWith' :: (Traversable f) => (Double -> Double -> b) -> (forall s. Reifies s Tape => f (ReverseDouble s) -> ReverseDouble s) -> f Double -> (Double, f b)
gradWith' g f as = reifyTape (snd bds) $ \p -> case f vs of
   r -> (primal r, unbindWith g vs $! partialArrayOf p bds $! r)
  where (vs, bds) = bind as
{-# INLINE gradWith' #-}

-- | The 'jacobian' function calculates the jacobian of a non-scalar-to-non-scalar function with reverse AD lazily in @m@ passes for @m@ outputs.
--
-- >>> jacobian (\[x,y] -> [y,x,x*y]) [2,1]
-- [[0.0,1.0],[1.0,0.0],[1.0,2.0]]
jacobian :: (Traversable f, Functor g) => (forall s. Reifies s Tape => f (ReverseDouble s) -> g (ReverseDouble s)) -> f Double -> g (f Double)
jacobian f as = reifyTape (snd bds) $ \p -> unbind vs . partialArrayOf p bds <$> f vs where
  (vs, bds) = bind as
{-# INLINE jacobian #-}

-- | The 'jacobian'' function calculates both the result and the Jacobian of a nonscalar-to-nonscalar function, using @m@ invocations of reverse AD,
-- where @m@ is the output dimensionality. Applying @fmap snd@ to the result will recover the result of 'jacobian'
-- | An alias for 'gradF''
--
-- >>> jacobian' (\[x,y] -> [y,x,x*y]) [2,1]
-- [(1.0,[0.0,1.0]),(2.0,[1.0,0.0]),(2.0,[1.0,2.0])]
jacobian' :: (Traversable f, Functor g) => (forall s. Reifies s Tape => f (ReverseDouble s) -> g (ReverseDouble s)) -> f Double -> g (Double, f Double)
jacobian' f as = reifyTape (snd bds) $ \p ->
  let row a = (primal a, unbind vs $! partialArrayOf p bds $! a)
  in row <$> f vs
  where (vs, bds) = bind as
{-# INLINE jacobian' #-}

-- | 'jacobianWith g f' calculates the Jacobian of a non-scalar-to-non-scalar function @f@ with reverse AD lazily in @m@ passes for @m@ outputs.
--
-- Instead of returning the Jacobian matrix, the elements of the matrix are combined with the input using the @g@.
--
-- @
-- 'jacobian' == 'jacobianWith' (\_ dx -> dx)
-- 'jacobianWith' 'const' == (\f x -> 'const' x '<$>' f x)
-- @
jacobianWith :: (Traversable f, Functor g) => (Double -> Double -> b) -> (forall s. Reifies s Tape => f (ReverseDouble s) -> g (ReverseDouble s)) -> f Double -> g (f b)
jacobianWith g f as = reifyTape (snd bds) $ \p -> unbindWith g vs . partialArrayOf p bds <$> f vs where
  (vs, bds) = bind as
{-# INLINE jacobianWith #-}

-- | 'jacobianWith' g f' calculates both the result and the Jacobian of a nonscalar-to-nonscalar function @f@, using @m@ invocations of reverse AD,
-- where @m@ is the output dimensionality. Applying @fmap snd@ to the result will recover the result of 'jacobianWith'
--
-- Instead of returning the Jacobian matrix, the elements of the matrix are combined with the input using the @g@.
--
-- @'jacobian'' == 'jacobianWith'' (\_ dx -> dx)@
--
jacobianWith' :: (Traversable f, Functor g) => (Double -> Double -> b) -> (forall s. Reifies s Tape => f (ReverseDouble s) -> g (ReverseDouble s)) -> f Double -> g (Double, f b)
jacobianWith' g f as = reifyTape (snd bds) $ \p ->
  let row a = (primal a, unbindWith g vs $! partialArrayOf p bds $! a)
  in row <$> f vs
  where (vs, bds) = bind as
{-# INLINE jacobianWith' #-}

-- | Compute the derivative of a function.
--
-- >>> diff sin 0
-- 1.0
diff :: (forall s. Reifies s Tape => ReverseDouble s -> ReverseDouble s) -> Double -> Double
diff f a = reifyTape 1 $ \p -> derivativeOf p $! f (var a 0)
{-# INLINE diff #-}

-- | The 'diff'' function calculates the result and derivative, as a pair, of a scalar-to-scalar function.
--
-- >>> diff' sin 0
-- (0.0,1.0)
--
-- >>> diff' exp 0
-- (1.0,1.0)
diff' :: (forall s. Reifies s Tape => ReverseDouble s -> ReverseDouble s) -> Double -> (Double, Double)
diff' f a = reifyTape 1 $ \p -> derivativeOf' p $! f (var a 0)
{-# INLINE diff' #-}

-- | Compute the derivatives of each result of a scalar-to-vector function with regards to its input.
--
-- >>> diffF (\a -> [sin a, cos a]) 0
-- [1.0,0.0]
--
diffF :: (Functor f) => (forall s. Reifies s Tape => ReverseDouble s -> f (ReverseDouble s)) -> Double -> f Double
diffF f a = reifyTape 1 $ \p -> derivativeOf p <$> f (var a 0)
{-# INLINE diffF #-}

-- | Compute the derivatives of each result of a scalar-to-vector function with regards to its input along with the answer.
--
-- >>> diffF' (\a -> [sin a, cos a]) 0
-- [(0.0,1.0),(1.0,0.0)]
diffF' :: (Functor f) => (forall s. Reifies s Tape => ReverseDouble s -> f (ReverseDouble s)) -> Double -> f (Double, Double)
diffF' f a = reifyTape 1 $ \p -> derivativeOf' p <$> f (var a 0)
{-# INLINE diffF' #-}

-- | Compute the hessian via the jacobian of the gradient. gradient is computed in reverse mode and then the jacobian is computed in reverse mode.
--
-- However, since the @'grad' f :: f a -> f a@ is square this is not as fast as using the forward-mode Jacobian of a reverse mode gradient provided by 'Numeric.AD.hessian'.
--
-- >>> hessian (\[x,y] -> x*y) [1,2]
-- [[0.0,1.0],[1.0,0.0]]
hessian :: (Traversable f) => (forall s s'. (Reifies s R.Tape, Reifies s' Tape) => f (On (R.Reverse s (ReverseDouble s'))) -> (On (R.Reverse s (ReverseDouble s')))) -> f Double -> f (f Double)
hessian f = jacobian (M.grad (off . f . fmap On))
{-# INLINE hessian #-}

-- | Compute the order 3 Hessian tensor on a non-scalar-to-non-scalar function via the reverse-mode Jacobian of the reverse-mode Jacobian of the function.
--
-- Less efficient than 'Numeric.AD.Mode.Mixed.hessianF'.
--
-- >>> hessianF (\[x,y] -> [x*y,x+y,exp x*cos y]) [1,2 :: Double]
-- [[[0.0,1.0],[1.0,0.0]],[[0.0,0.0],[0.0,0.0]],[[-1.1312043837568135,-2.4717266720048188],[-2.4717266720048188,1.1312043837568135]]]
hessianF :: (Traversable f, Functor g) => (forall s s'. (Reifies s R.Tape, Reifies s' Tape) => f (On (R.Reverse s (ReverseDouble s'))) -> g (On (R.Reverse s (ReverseDouble s')))) -> f Double -> g (f (f Double))
hessianF f = getCompose . jacobian (Compose . M.jacobian (fmap off . f . fmap On))
{-# INLINE hessianF #-}
