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

module Numeric.AD.Mode.Kahn
  ( AD, Kahn, auto
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

import Numeric.AD.Internal.Kahn (Kahn)
import Numeric.AD.Internal.On
import Numeric.AD.Internal.Type (AD(..))
import Numeric.AD.Mode
import qualified Numeric.AD.Rank1.Kahn as Rank1

-- $setup
--
-- >>> import Numeric.AD.Internal.Doctest

-- | The 'grad' function calculates the gradient of a non-scalar-to-scalar function with kahn-mode AD in a single pass.
--
--
-- >>> grad (\[x,y,z] -> x*y+z) [1,2,3]
-- [2,1,1]
--
-- >>> grad (\[x,y] -> x**y) [0,2]
-- [0.0,NaN]
grad :: (Traversable f, Num a) => (forall s. f (AD s (Kahn a)) -> AD s (Kahn a)) -> f a -> f a
grad f = Rank1.grad (runAD.f.fmap AD)
{-# INLINE grad #-}

-- | The 'grad'' function calculates the result and gradient of a non-scalar-to-scalar function with kahn-mode AD in a single pass.
--
-- >>> grad' (\[x,y,z] -> 4*x*exp y+cos z) [1,2,3]
-- (28.566231899122155,[29.5562243957226,29.5562243957226,-0.1411200080598672])
grad' :: (Traversable f, Num a) => (forall s. f (AD s (Kahn a)) -> AD s (Kahn a)) -> f a -> (a, f a)
grad' f = Rank1.grad' (runAD.f.fmap AD)
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
gradWith :: (Traversable f, Num a) => (a -> a -> b) -> (forall s. f (AD s (Kahn a)) -> AD s (Kahn a)) -> f a -> f b
gradWith g f = Rank1.gradWith g (runAD.f.fmap AD)
{-# INLINE gradWith #-}

-- | @'grad'' g f@ calculates the result and gradient of a non-scalar-to-scalar function @f@ with kahn-mode AD in a single pass
-- the gradient is combined element-wise with the argument using the function @g@.
--
-- @'grad'' == 'gradWith'' (\_ dx -> dx)@
gradWith' :: (Traversable f, Num a) => (a -> a -> b) -> (forall s. f (AD s (Kahn a)) -> AD s (Kahn a)) -> f a -> (a, f b)
gradWith' g f = Rank1.gradWith' g (runAD.f.fmap AD)
{-# INLINE gradWith' #-}

-- | The 'jacobian' function calculates the jacobian of a non-scalar-to-non-scalar function with kahn AD lazily in @m@ passes for @m@ outputs.
--
-- >>> jacobian (\[x,y] -> [y,x,x*y]) [2,1]
-- [[0,1],[1,0],[1,2]]
--
-- >>> jacobian (\[x,y] -> [exp y,cos x,x+y]) [1,2]
-- [[0.0,7.38905609893065],[-0.8414709848078965,0.0],[1.0,1.0]]
jacobian :: (Traversable f, Functor g, Num a) => (forall s. f (AD s (Kahn a)) -> g (AD s (Kahn a))) -> f a -> g (f a)
jacobian f = Rank1.jacobian (fmap runAD.f.fmap AD)
{-# INLINE jacobian #-}

-- | The 'jacobian'' function calculates both the result and the Jacobian of a nonscalar-to-nonscalar function, using @m@ invocations of kahn AD,
-- where @m@ is the output dimensionality. Applying @fmap snd@ to the result will recover the result of 'jacobian'
-- | An alias for 'gradF''
--
-- ghci> jacobian' (\[x,y] -> [y,x,x*y]) [2,1]
-- [(1,[0,1]),(2,[1,0]),(2,[1,2])]
jacobian' :: (Traversable f, Functor g, Num a) => (forall s. f (AD s (Kahn a)) -> g (AD s (Kahn a))) -> f a -> g (a, f a)
jacobian' f = Rank1.jacobian' (fmap runAD.f.fmap AD)
{-# INLINE jacobian' #-}

-- | 'jacobianWith g f' calculates the Jacobian of a non-scalar-to-non-scalar function @f@ with kahn AD lazily in @m@ passes for @m@ outputs.
--
-- Instead of returning the Jacobian matrix, the elements of the matrix are combined with the input using the @g@.
--
-- @
-- 'jacobian' = 'jacobianWith' (\_ dx -> dx)
-- 'jacobianWith' 'const' = (\f x -> 'const' x '<$>' f x)
-- @
jacobianWith :: (Traversable f, Functor g, Num a) => (a -> a -> b) -> (forall s. f (AD s (Kahn a)) -> g (AD s (Kahn a))) -> f a -> g (f b)
jacobianWith g f = Rank1.jacobianWith g (fmap runAD.f.fmap AD)
{-# INLINE jacobianWith #-}

-- | 'jacobianWith' g f' calculates both the result and the Jacobian of a nonscalar-to-nonscalar function @f@, using @m@ invocations of kahn AD,
-- where @m@ is the output dimensionality. Applying @fmap snd@ to the result will recover the result of 'jacobianWith'
--
-- Instead of returning the Jacobian matrix, the elements of the matrix are combined with the input using the @g@.
--
-- @'jacobian'' == 'jacobianWith'' (\_ dx -> dx)@
jacobianWith' :: (Traversable f, Functor g, Num a) => (a -> a -> b) -> (forall s. f (AD s (Kahn a)) -> g (AD s (Kahn a))) -> f a -> g (a, f b)
jacobianWith' g f = Rank1.jacobianWith' g (fmap runAD.f.fmap AD)
{-# INLINE jacobianWith' #-}

-- | Compute the derivative of a function.
--
-- >>> diff sin 0
-- 1.0
--
-- >>> cos 0
-- 1.0
diff :: Num a => (forall s. AD s (Kahn a) -> AD s (Kahn a)) -> a -> a
diff f = Rank1.diff (runAD.f.AD)
{-# INLINE diff #-}

-- | The 'diff'' function calculates the value and derivative, as a
-- pair, of a scalar-to-scalar function.
--
--
-- >>> diff' sin 0
-- (0.0,1.0)
diff' :: Num a => (forall s. AD s (Kahn a) -> AD s (Kahn a)) -> a -> (a, a)
diff' f = Rank1.diff' (runAD.f.AD)
{-# INLINE diff' #-}

-- | Compute the derivatives of a function that returns a vector with regards to its single input.
--
-- >>> diffF (\a -> [sin a, cos a]) 0
-- [1.0,0.0]
diffF :: (Functor f, Num a) => (forall s. AD s (Kahn a) -> f (AD s (Kahn a))) -> a -> f a
diffF f = Rank1.diffF (fmap runAD.f.AD)
{-# INLINE diffF #-}

-- | Compute the derivatives of a function that returns a vector with regards to its single input
-- as well as the primal answer.
--
-- >>> diffF' (\a -> [sin a, cos a]) 0
-- [(0.0,1.0),(1.0,0.0)]
diffF' :: (Functor f, Num a) => (forall s. AD s (Kahn a) -> f (AD s (Kahn a))) -> a -> f (a, a)
diffF' f = Rank1.diffF' (fmap runAD.f.AD)
{-# INLINE diffF' #-}

-- | Compute the 'hessian' via the 'jacobian' of the gradient. gradient is computed in 'Kahn' mode and then the 'jacobian' is computed in 'Kahn' mode.
--
-- However, since the @'grad' f :: f a -> f a@ is square this is not as fast as using the forward-mode 'jacobian' of a reverse mode gradient provided by 'Numeric.AD.hessian'.
--
-- >>> hessian (\[x,y] -> x*y) [1,2]
-- [[0,1],[1,0]]
hessian :: (Traversable f, Num a) => (forall s. f (AD s (On (Kahn (Kahn a)))) -> AD s (On (Kahn (Kahn a)))) -> f a -> f (f a)
hessian f = Rank1.hessian (runAD.f.fmap AD)

-- | Compute the order 3 Hessian tensor on a non-scalar-to-non-scalar function via the 'Kahn'-mode Jacobian of the 'Kahn'-mode Jacobian of the function.
--
-- Less efficient than 'Numeric.AD.Mode.Mixed.hessianF'.
--
-- >>> hessianF (\[x,y] -> [x*y,x+y,exp x*cos y]) [1,2 :: RDouble]
-- [[[0.0,1.0],[1.0,0.0]],[[0.0,0.0],[0.0,0.0]],[[-1.131204383757,-2.471726672005],[-2.471726672005,1.131204383757]]]
hessianF :: (Traversable f, Functor g, Num a) => (forall s. f (AD s (On (Kahn (Kahn a)))) -> g (AD s (On (Kahn (Kahn a))))) -> f a -> g (f (f a))
hessianF f = Rank1.hessianF (fmap runAD.f.fmap AD)
