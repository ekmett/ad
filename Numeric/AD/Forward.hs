{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Forward
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Forward mode automatic differentiation
--
-----------------------------------------------------------------------------

module Numeric.AD.Forward
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
    -- * Monadic Combinators
    , diffM
    , diffM'
    -- * Exposed Types
    , UU, UF, FU, FF
    , AD(..)
    , Mode(..)
    ) where

import Data.Traversable (Traversable)
import Control.Applicative
import Control.Monad (liftM)
import Numeric.AD.Internal
import Numeric.AD.Internal.Composition
import Numeric.AD.Internal.Forward

du :: (Functor f, Num a) => FU f a -> f (a, a) -> a
du f = tangent . f . fmap (uncurry bundle)
{-# INLINE du #-}

du' :: (Functor f, Num a) => FU f a -> f (a, a) -> (a, a)
du' f = unbundle . f . fmap (uncurry bundle)
{-# INLINE du' #-}

duF :: (Functor f, Functor g, Num a) => FF f g a -> f (a, a) -> g a
duF f = fmap tangent . f . fmap (uncurry bundle)
{-# INLINE duF #-}

duF' :: (Functor f, Functor g, Num a) => FF f g a -> f (a, a) -> g (a, a)
duF' f = fmap unbundle . f . fmap (uncurry bundle)
{-# INLINE duF' #-}

-- | The 'diff' function calculates the first derivative of a scalar-to-scalar function by forward-mode 'AD'
--
-- > diff sin == cos
diff :: Num a => UU a -> a -> a
diff f a = tangent $ apply f a
{-# INLINE diff #-}

-- | The 'd'UU' function calculates the result and first derivative of scalar-to-scalar function by F'orward' 'AD'
-- 
-- > d' sin == sin &&& cos
-- > d' f = f &&& d f
diff' :: Num a => UU a -> a -> (a, a)
diff' f a = unbundle $ apply f a
{-# INLINE diff' #-}

-- | The 'diffF' function calculates the first derivative of scalar-to-nonscalar function by F'orward' 'AD'
diffF :: (Functor f, Num a) => UF f a -> a -> f a
diffF f a = tangent <$> apply f a
{-# INLINE diffF #-}

-- | The 'diffF'' function calculates the result and first derivative of a scalar-to-non-scalar function by F'orward' 'AD'
diffF' :: (Functor f, Num a) => UF f a -> a -> f (a, a)
diffF' f a = unbundle <$> apply f a
{-# INLINE diffF' #-}

-- | The 'dUM' function calculates the first derivative of scalar-to-scalar monadic function by F'orward' 'AD'
diffM :: (Monad m, Num a) => UF m a -> a -> m a
diffM f a = tangent `liftM` apply f a
{-# INLINE diffM #-}

-- | The 'd'UM' function calculates the result and first derivative of a scalar-to-scalar monadic function by F'orward' 'AD'
diffM' :: (Monad m, Num a) => UF m a -> a -> m (a, a)
diffM' f a = unbundle `liftM` apply f a
{-# INLINE diffM' #-}

-- | A fast, simple transposed Jacobian computed with forward-mode AD.
jacobianT :: (Traversable f, Functor g, Num a) => FF f g a -> f a -> f (g a)
jacobianT f = bind (fmap tangent . f)
{-# INLINE jacobianT #-}

-- | A fast, simple transposed Jacobian computed with forward-mode AD.
jacobianWithT :: (Traversable f, Functor g, Num a) => (a -> a -> b) -> FF f g a -> f a -> f (g b)
jacobianWithT g f = bindWith g' f
    where g' a ga = g a . tangent <$> ga
{-# INLINE jacobianWithT #-}

jacobian :: (Traversable f, Traversable g, Num a) => FF f g a -> f a -> g (f a)
jacobian f as = transposeWith (const id) t p
    where
        (p, t) = bind' (fmap tangent . f) as
{-# INLINE jacobian #-}

jacobianWith :: (Traversable f, Traversable g, Num a) => (a -> a -> b) -> FF f g a -> f a -> g (f b)
jacobianWith g f as = transposeWith (const id) t p
    where
        (p, t) = bindWith' g' f as
        g' a ga = g a . tangent <$> ga
{-# INLINE jacobianWith #-}

jacobian' :: (Traversable f, Traversable g, Num a) => FF f g a -> f a -> g (a, f a)
jacobian' f as = transposeWith row t p
    where
        (p, t) = bind' f as
        row x as' = (primal x, tangent <$> as')
{-# INLINE jacobian' #-}

jacobianWith' :: (Traversable f, Traversable g, Num a) => (a -> a -> b) -> FF f g a -> f a -> g (a, f b)
jacobianWith' g f as = transposeWith row t p
    where
        (p, t) = bindWith' g' f as
        row x as' = (primal x, as')
        g' a ga = g a . tangent <$> ga
{-# INLINE jacobianWith' #-}

grad :: (Traversable f, Num a) => FU f a -> f a -> f a
grad f = bind (tangent . f)
{-# INLINE grad #-}

grad' :: (Traversable f, Num a) => FU f a -> f a -> (a, f a)
grad' f as = (primal b, tangent <$> bs)
    where
        (b, bs) = bind' f as
{-# INLINE grad' #-}

gradWith :: (Traversable f, Num a) => (a -> a -> b) -> FU f a -> f a -> f b
gradWith g f = bindWith g (tangent . f)
{-# INLINE gradWith #-}

gradWith' :: (Traversable f, Num a) => (a -> a -> b) -> FU f a -> f a -> (a, f b)
gradWith' g f = bindWith' g (tangent . f)
{-# INLINE gradWith' #-}

-- | Compute the product of a vector with the Hessian using forward-on-forward-mode AD. 
hessianProduct :: (Traversable f, Num a) => FU f a -> f (a, a) -> f a
hessianProduct f = duF $ grad $ decomposeMode . f . fmap composeMode

-- | Compute the gradient and hessian product using forward-on-forward-mode AD. 
hessianProduct' :: (Traversable f, Num a) => FU f a -> f (a, a) -> f (a, a)
hessianProduct' f = duF' $ grad $ decomposeMode . f . fmap composeMode

-- * Experimental

-- data f :> a = a :< f (f :> a)
-- gradients :: (Traversable f, Num a) => FU f a -> f a -> (f :> a)
