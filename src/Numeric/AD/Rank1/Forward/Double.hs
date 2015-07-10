{-# LANGUAGE CPP #-}
module Numeric.AD.Rank1.Forward.Double
  ( ForwardDouble
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

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
import Data.Traversable (Traversable)
#endif
import Numeric.AD.Mode
import Numeric.AD.Internal.Forward.Double

-- | Compute the directional derivative of a function given a zipped up 'Functor' of the input values and their derivatives
du :: Functor f => (f ForwardDouble -> ForwardDouble) -> f (Double, Double) -> Double
du f = tangent . f . fmap (uncurry bundle)
{-# INLINE du #-}

-- | Compute the answer and directional derivative of a function given a zipped up 'Functor' of the input values and their derivatives
du' :: Functor f => (f ForwardDouble -> ForwardDouble) -> f (Double, Double) -> (Double, Double)
du' f = unbundle . f . fmap (uncurry bundle)
{-# INLINE du' #-}

-- | Compute a vector of directional derivatives for a function given a zipped up 'Functor' of the input values and their derivatives.
duF :: (Functor f, Functor g) => (f ForwardDouble -> g ForwardDouble) -> f (Double, Double) -> g Double
duF f = fmap tangent . f . fmap (uncurry bundle)
{-# INLINE duF #-}

-- | Compute a vector of answers and directional derivatives for a function given a zipped up 'Functor' of the input values and their derivatives.
duF' :: (Functor f, Functor g) => (f ForwardDouble -> g ForwardDouble) -> f (Double, Double) -> g (Double, Double)
duF' f = fmap unbundle . f . fmap (uncurry bundle)
{-# INLINE duF' #-}

-- | The 'diff' function calculates the first derivative of a scalar-to-scalar function by forward-mode 'AD'
--
-- >>> diff sin 0
-- 1.0
diff :: (ForwardDouble -> ForwardDouble) -> Double -> Double
diff f a = tangent $ apply f a
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
diff' :: (ForwardDouble -> ForwardDouble) -> Double -> (Double, Double)
diff' f a = unbundle $ apply f a
{-# INLINE diff' #-}

-- | The 'diffF' function calculates the first derivatives of scalar-to-nonscalar function by 'Forward' mode 'AD'
--
-- >>> diffF (\a -> [sin a, cos a]) 0
-- [1.0,-0.0]
diffF :: Functor f => (ForwardDouble -> f ForwardDouble) -> Double -> f Double
diffF f a = tangent <$> apply f a
{-# INLINE diffF #-}

-- | The 'diffF'' function calculates the result and first derivatives of a scalar-to-non-scalar function by 'Forward' mode 'AD'
--
-- >>> diffF' (\a -> [sin a, cos a]) 0
-- [(0.0,1.0),(1.0,-0.0)]
diffF' :: Functor f => (ForwardDouble -> f ForwardDouble) -> Double -> f (Double, Double)
diffF' f a = unbundle <$> apply f a
{-# INLINE diffF' #-}

-- | A fast, simple, transposed Jacobian computed with forward-mode AD.
jacobianT :: (Traversable f, Functor g) => (f ForwardDouble -> g ForwardDouble) -> f Double -> f (g Double)
jacobianT f = bind (fmap tangent . f)
{-# INLINE jacobianT #-}

-- | A fast, simple, transposed Jacobian computed with 'Forward' mode 'AD' that combines the output with the input.
jacobianWithT :: (Traversable f, Functor g) => (Double -> Double -> b) -> (f ForwardDouble -> g ForwardDouble) -> f Double -> f (g b)
jacobianWithT g f = bindWith g' f where
  g' a ga = g a . tangent <$> ga
{-# INLINE jacobianWithT #-}
{-# ANN jacobianWithT "HLint: ignore Eta reduce" #-}

-- | Compute the Jacobian using 'Forward' mode 'AD'. This must transpose the result, so 'jacobianT' is faster and allows more result types.
--
--
-- >>> jacobian (\[x,y] -> [y,x,x+y,x*y,exp x * sin y]) [pi,1]
-- [[0.0,1.0],[1.0,0.0],[1.0,1.0],[1.0,3.141592653589793],[19.472221418841606,12.502969588876512]]
jacobian :: (Traversable f, Traversable g) => (f ForwardDouble -> g ForwardDouble) -> f Double -> g (f Double)
jacobian f as = transposeWith (const id) t p where
  (p, t) = bind' (fmap tangent . f) as
{-# INLINE jacobian #-}

-- | Compute the Jacobian using 'Forward' mode 'AD' and combine the output with the input. This must transpose the result, so 'jacobianWithT' is faster, and allows more result types.
jacobianWith :: (Traversable f, Traversable g) => (Double -> Double -> b) -> (f ForwardDouble -> g ForwardDouble) -> f Double -> g (f b)
jacobianWith g f as = transposeWith (const id) t p where
  (p, t) = bindWith' g' f as
  g' a ga = g a . tangent <$> ga
{-# INLINE jacobianWith #-}

-- | Compute the Jacobian using 'Forward' mode 'AD' along with the actual answer.
jacobian' :: (Traversable f, Traversable g) => (f ForwardDouble -> g ForwardDouble) -> f Double -> g (Double, f Double)
jacobian' f as = transposeWith row t p where
  (p, t) = bind' f as
  row x as' = (primal x, tangent <$> as')
{-# INLINE jacobian' #-}

-- | Compute the Jacobian using 'Forward' mode 'AD' combined with the input using a user specified function, along with the actual answer.
jacobianWith' :: (Traversable f, Traversable g) => (Double -> Double -> b) -> (f ForwardDouble -> g ForwardDouble) -> f Double -> g (Double, f b)
jacobianWith' g f as = transposeWith row t p where
  (p, t) = bindWith' g' f as
  row x as' = (primal x, as')
  g' a ga = g a . tangent <$> ga
{-# INLINE jacobianWith' #-}

-- | Compute the gradient of a function using forward mode AD.
--
-- Note, this performs /O(n)/ worse than 'Numeric.AD.Mode.Wengert.grad' for @n@ inputs, in exchange for better space utilization.
grad :: Traversable f => (f ForwardDouble -> ForwardDouble) -> f Double -> f Double
grad f = bind (tangent . f)
{-# INLINE grad #-}

-- | Compute the gradient and answer to a function using forward mode AD.
--
-- Note, this performs /O(n)/ worse than 'Numeric.AD.Mode.Wengert.grad'' for @n@ inputs, in exchange for better space utilization.
grad' :: Traversable f => (f ForwardDouble -> ForwardDouble) -> f Double -> (Double, f Double)
grad' f as = (primal b, tangent <$> bs)
    where
        (b, bs) = bind' f as
{-# INLINE grad' #-}

-- | Compute the gradient of a function using forward mode AD and combine the result with the input using a user-specified function.
--
-- Note, this performs /O(n)/ worse than 'Numeric.AD.Mode.Wengert.gradWith' for @n@ inputs, in exchange for better space utilization.
gradWith :: Traversable f => (Double -> Double -> b) -> (f ForwardDouble -> ForwardDouble) -> f Double -> f b
gradWith g f = bindWith g (tangent . f)
{-# INLINE gradWith #-}

-- | Compute the gradient of a function using forward mode AD and the answer, and combine the result with the input using a
-- user-specified function.
--
-- Note, this performs /O(n)/ worse than 'Numeric.AD.Mode.Wengert.gradWith'' for @n@ inputs, in exchange for better space utilization.
--
-- >>> gradWith' (,) sum [0..4]
-- (10.0,[(0.0,1.0),(1.0,1.0),(2.0,1.0),(3.0,1.0),(4.0,1.0)])
gradWith' :: Traversable f => (Double -> Double -> b) -> (f ForwardDouble -> ForwardDouble) -> f Double -> (Double, f b)
gradWith' g f as = (primal $ f (auto <$> as), bindWith g (tangent . f) as)
{-# INLINE gradWith' #-}
