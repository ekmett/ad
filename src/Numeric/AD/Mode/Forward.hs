{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2010-2021
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Forward mode automatic differentiation
--
-----------------------------------------------------------------------------

module Numeric.AD.Mode.Forward
  ( AD
  , Forward
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
  -- * Transposed Jacobian
  , jacobianT
  , jacobianWithT
  -- * Hessian Product
  , hessianProduct
  , hessianProduct'
  -- * Derivatives
  , diff
  , diff'
  , diffF
  , diffF'
  -- * Directional Derivatives
  , du
  , du'
  , duF
  , duF'
  ) where

import Numeric.AD.Internal.Forward
import Numeric.AD.Internal.On
import Numeric.AD.Internal.Type
import qualified Numeric.AD.Rank1.Forward as Rank1
import Numeric.AD.Mode

-- | Compute the directional derivative of a function given a zipped up 'Functor' of the input values and their derivatives
du :: (Functor f, Num a) => (forall s. f (AD s (Forward a)) -> AD s (Forward a)) -> f (a, a) -> a
du f = Rank1.du (runAD.f.fmap AD)
{-# INLINE du #-}

-- | Compute the answer and directional derivative of a function given a zipped up 'Functor' of the input values and their derivatives
du' :: (Functor f, Num a) => (forall s. f (AD s (Forward a)) -> AD s (Forward a)) -> f (a, a) -> (a, a)
du' f = Rank1.du' (runAD.f.fmap AD)
{-# INLINE du' #-}

-- | Compute a vector of directional derivatives for a function given a zipped up 'Functor' of the input values and their derivatives.
duF :: (Functor f, Functor g, Num a) => (forall s. f (AD s (Forward a)) -> g (AD s (Forward a))) -> f (a, a) -> g a
duF f = Rank1.duF (fmap runAD.f.fmap AD)
{-# INLINE duF #-}

-- | Compute a vector of answers and directional derivatives for a function given a zipped up 'Functor' of the input values and their derivatives.
duF' :: (Functor f, Functor g, Num a) => (forall s. f (AD s (Forward a)) -> g (AD s (Forward a))) -> f (a, a) -> g (a, a)
duF' f = Rank1.duF' (fmap runAD.f.fmap AD)
{-# INLINE duF' #-}

-- | The 'diff' function calculates the first derivative of a scalar-to-scalar function by forward-mode 'AD'
--
-- >>> diff sin 0
-- 1.0
diff :: Num a => (forall s. AD s (Forward a) -> AD s (Forward a)) -> a -> a
diff f = Rank1.diff (runAD.f.AD)
{-# INLINE diff #-}

-- | The 'diff'' function calculates the result and first derivative of scalar-to-scalar function by 'Forward' mode 'AD'
--
-- @
-- 'diff'' 'sin' == 'sin' 'Control.Arrow.&&&' 'cos'
-- 'diff'' f = f 'Control.Arrow.&&&' d f
-- @
--
-- >>> diff' sin 0
-- (0.0,1.0)
--
-- >>> diff' exp 0
-- (1.0,1.0)

diff' :: Num a => (forall s. AD s (Forward a) -> AD s (Forward a)) -> a -> (a, a)
diff' f = Rank1.diff' (runAD.f.AD)
{-# INLINE diff' #-}

-- | The 'diffF' function calculates the first derivatives of scalar-to-nonscalar function by 'Forward' mode 'AD'
--
-- >>> diffF (\a -> [sin a, cos a]) 0
-- [1.0,-0.0]
diffF :: (Functor f, Num a) => (forall s. AD s (Forward a) -> f (AD s (Forward a))) -> a -> f a
diffF f = Rank1.diffF (fmap runAD.f.AD)
{-# INLINE diffF #-}

-- | The 'diffF'' function calculates the result and first derivatives of a scalar-to-non-scalar function by 'Forward' mode 'AD'
--
-- >>> diffF' (\a -> [sin a, cos a]) 0
-- [(0.0,1.0),(1.0,-0.0)]
diffF' :: (Functor f, Num a) => (forall s. AD s (Forward a) -> f (AD s (Forward a))) -> a -> f (a, a)
diffF' f = Rank1.diffF' (fmap runAD.f.AD)
{-# INLINE diffF' #-}

-- | A fast, simple, transposed Jacobian computed with forward-mode AD.
jacobianT :: (Traversable f, Functor g, Num a) => (forall s. f (AD s (Forward a)) -> g (AD s (Forward a))) -> f a -> f (g a)
jacobianT f = Rank1.jacobianT (fmap runAD.f.fmap AD)
{-# INLINE jacobianT #-}

-- | A fast, simple, transposed Jacobian computed with 'Forward' mode 'AD' that combines the output with the input.
jacobianWithT :: (Traversable f, Functor g, Num a) => (a -> a -> b) -> (forall s. f (AD s (Forward a)) -> g (AD s (Forward a))) -> f a -> f (g b)
jacobianWithT g f = Rank1.jacobianWithT g (fmap runAD.f.fmap AD)
{-# INLINE jacobianWithT #-}

-- | Compute the Jacobian using 'Forward' mode 'AD'. This must transpose the result, so 'jacobianT' is faster and allows more result types.
--
--
-- >>> jacobian (\[x,y] -> [y,x,x+y,x*y,exp x * sin y]) [pi,1]
-- [[0.0,1.0],[1.0,0.0],[1.0,1.0],[1.0,3.141592653589793],[19.472221418841606,12.502969588876512]]
jacobian :: (Traversable f, Traversable g, Num a) => (forall s . f (AD s (Forward a)) -> g (AD s (Forward a))) -> f a -> g (f a)
jacobian f = Rank1.jacobian (fmap runAD.f.fmap AD)
{-# INLINE jacobian #-}

-- | Compute the Jacobian using 'Forward' mode 'AD' and combine the output with the input. This must transpose the result, so 'jacobianWithT' is faster, and allows more result types.
jacobianWith :: (Traversable f, Traversable g, Num a) => (a -> a -> b) -> (forall s. f (AD s (Forward a)) -> g (AD s (Forward a))) -> f a -> g (f b)
jacobianWith g f = Rank1.jacobianWith g (fmap runAD.f.fmap AD)
{-# INLINE jacobianWith #-}

-- | Compute the Jacobian using 'Forward' mode 'AD' along with the actual answer.
jacobian' :: (Traversable f, Traversable g, Num a) => (forall s. f (AD s (Forward a)) -> g (AD s (Forward a))) -> f a -> g (a, f a)
jacobian' f = Rank1.jacobian' (fmap runAD.f.fmap AD)
{-# INLINE jacobian' #-}

-- | Compute the Jacobian using 'Forward' mode 'AD' combined with the input using a user specified function, along with the actual answer.
jacobianWith' :: (Traversable f, Traversable g, Num a) => (a -> a -> b) -> (forall s. f (AD s (Forward a)) -> g (AD s (Forward a))) -> f a -> g (a, f b)
jacobianWith' g f = Rank1.jacobianWith' g (fmap runAD.f.fmap AD)
{-# INLINE jacobianWith' #-}

-- | Compute the gradient of a function using forward mode AD.
--
-- Note, this performs /O(n)/ worse than 'Numeric.AD.Mode.Reverse.grad' for @n@ inputs, in exchange for better space utilization.
grad :: (Traversable f, Num a) => (forall s. f (AD s (Forward a)) -> AD s (Forward a)) -> f a -> f a
grad f = Rank1.grad (runAD.f.fmap AD)
{-# INLINE grad #-}

-- | Compute the gradient and answer to a function using forward mode AD.
--
-- Note, this performs /O(n)/ worse than 'Numeric.AD.Mode.Reverse.grad'' for @n@ inputs, in exchange for better space utilization.
grad' :: (Traversable f, Num a) => (forall s. f (AD s (Forward a)) -> AD s (Forward a)) -> f a -> (a, f a)
grad' f = Rank1.grad' (runAD.f.fmap AD)
{-# INLINE grad' #-}

-- | Compute the gradient of a function using forward mode AD and combine the result with the input using a user-specified function.
--
-- Note, this performs /O(n)/ worse than 'Numeric.AD.Mode.Reverse.gradWith' for @n@ inputs, in exchange for better space utilization.
gradWith :: (Traversable f, Num a) => (a -> a -> b) -> (forall s. f (AD s (Forward a)) -> AD s (Forward a)) -> f a -> f b
gradWith g f = Rank1.gradWith g (runAD.f.fmap AD)
{-# INLINE gradWith #-}

-- | Compute the gradient of a function using forward mode AD and the answer, and combine the result with the input using a
-- user-specified function.
--
-- Note, this performs /O(n)/ worse than 'Numeric.AD.Mode.Reverse.gradWith'' for @n@ inputs, in exchange for better space utilization.
--
-- >>> gradWith' (,) sum [0..4]
-- (10,[(0,1),(1,1),(2,1),(3,1),(4,1)])
gradWith' :: (Traversable f, Num a) => (a -> a -> b) -> (forall s. f (AD s (Forward a)) -> AD s (Forward a)) -> f a -> (a, f b)
gradWith' g f = Rank1.gradWith' g (runAD.f.fmap AD)
{-# INLINE gradWith' #-}

-- | Compute the product of a vector with the Hessian using forward-on-forward-mode AD.
--
hessianProduct :: (Traversable f, Num a) => (forall s. f (AD s (On (Forward (Forward a)))) -> AD s (On (Forward (Forward a)))) -> f (a, a) -> f a
hessianProduct f = Rank1.hessianProduct (runAD.f.fmap AD)
{-# INLINE hessianProduct #-}

-- | Compute the gradient and hessian product using forward-on-forward-mode AD.
hessianProduct' :: (Traversable f, Num a) => (forall s. f (AD s (On (Forward (Forward a)))) -> AD s (On (Forward (Forward a)))) -> f (a, a) -> f (a, a)
hessianProduct' f = Rank1.hessianProduct' (runAD.f.fmap AD)
{-# INLINE hessianProduct' #-}
