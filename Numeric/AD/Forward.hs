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
    , grad2
    , gradWith
    , gradWith2
    -- * Jacobian
    , jacobian
    , jacobian2
    , jacobianT
    , jacobianWith
    , jacobianWith2
    , jacobianWithT
    -- * Derivatives
    , diffUU
    , diff2UU
    , diffUF
    , diff2UF
    -- * Directional Derivatives
    , diffMU
    , diff2MU
    , diffMF
    , diff2MF
    -- * Monadic Combinators
    , diffUM
    , diff2UM
    -- * Synonyms
    , diff
    , diff2
    -- * Exposed Types
    , AD(..)
    , Mode(..)
    ) where

import Data.Traversable (Traversable)
import Control.Applicative
import Control.Monad (liftM)
import Numeric.AD.Classes
import Numeric.AD.Internal
import Numeric.AD.Internal.Forward

diffMU :: (Functor f, Num a) => (forall s. Mode s => f (AD s a) -> AD s a) -> f (a, a) -> a
diffMU f = tangent . f . fmap (uncurry bundle)
{-# INLINE diffMU #-}

diff2MU :: (Functor f, Num a) => (forall s. Mode s => f (AD s a) -> AD s a) -> f (a, a) -> (a, a)
diff2MU f = unbundle . f . fmap (uncurry bundle)
{-# INLINE diff2MU #-}

diffMF :: (Functor f, Functor g, Num a) => (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f (a, a) -> g a
diffMF f = fmap tangent . f . fmap (uncurry bundle)
{-# INLINE diffMF #-}

diff2MF :: (Functor f, Functor g, Num a) => (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f (a, a) -> g (a, a)
diff2MF f = fmap unbundle . f . fmap (uncurry bundle)
{-# INLINE diff2MF #-}

-- | The 'diff2' function calculates the first derivative of scalar-to-scalar function by 'Forward' 'AD'
diff :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> a
diff = diffUU
{-# INLINE diff #-}

-- | The 'diff2' function calculates the result and first derivative of scalar-to-scalar function by 'Forward' 'AD'
diff2 :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> (a, a)
diff2 = diff2UU
{-# INLINE diff2 #-}

-- | The 'diffUU' function calculates the first derivative of a scalar-to-scalar function by 'Forward' 'AD'
diffUU :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> a
diffUU f a = tangent $ apply f a
{-# INLINE diffUU #-}

-- | The 'diff2UU' function calculates the result and first derivative of scalar-to-scalar function by 'Forward' 'AD'
diff2UU :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> (a, a)
diff2UU f a = unbundle $ apply f a
{-# INLINE diff2UU #-}

-- | The 'diffUF' function calculates the first derivative of scalar-to-nonscalar function by 'Forward' 'AD'
diffUF :: (Functor f, Num a) => (forall s. Mode s => AD s a -> f (AD s a)) -> a -> f a
diffUF f a = tangent <$> apply f a
{-# INLINE diffUF #-}

-- | The 'diff2UF' function calculates the result and first derivative of a scalar-to-non-scalar function by 'Forward' 'AD'
diff2UF :: (Functor f, Num a) => (forall s. Mode s => AD s a -> f (AD s a)) -> a -> f (a, a)
diff2UF f a = unbundle <$> apply f a
{-# INLINE diff2UF #-}

-- | The 'diffUM' function calculates the first derivative of scalar-to-scalar monadic function by 'Forward' 'AD'
diffUM :: (Monad m, Num a) => (forall s. Mode s => AD s a -> m (AD s a)) -> a -> m a
diffUM f a = tangent `liftM` apply f a
{-# INLINE diffUM #-}

-- | The 'diff2UM' function calculates the result and first derivative of a scalar-to-scalar monadic function by 'Forward' 'AD'
diff2UM :: (Monad m, Num a) => (forall s. Mode s => AD s a -> m (AD s a)) -> a -> m (a, a)
diff2UM f a = unbundle `liftM` apply f a
{-# INLINE diff2UM #-}

-- | A fast, simple transposed Jacobian computed with forward-mode AD.
jacobianT :: (Traversable f, Functor g, Num a) => (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> f (g a)
jacobianT f = bind (fmap tangent . f)
{-# INLINE jacobianT #-}

-- | A fast, simple transposed Jacobian computed with forward-mode AD.
jacobianWithT :: (Traversable f, Functor g, Num a) => (a -> a -> b) -> (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> f (g b)
jacobianWithT g f = bindWith g' f
    where g' a ga = g a . tangent <$> ga
{-# INLINE jacobianWithT #-}

jacobian :: (Traversable f, Traversable g, Num a) => (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (f a)
jacobian f as = transposeWith (const id) t p
    where
        (p, t) = bind2 (fmap tangent . f) as
{-# INLINE jacobian #-}

jacobianWith :: (Traversable f, Traversable g, Num a) => (a -> a -> b) -> (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (f b)
jacobianWith g f as = transposeWith (const id) t p
    where
        (p, t) = bindWith2 g' f as
        g' a ga = g a . tangent <$> ga
{-# INLINE jacobianWith #-}

jacobian2 :: (Traversable f, Traversable g, Num a) => (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (a, f a)
jacobian2 f as = transposeWith row t p
    where
        (p, t) = bind2 f as
        row x as' = (primal x, tangent <$> as')
{-# INLINE jacobian2 #-}

jacobianWith2 :: (Traversable f, Traversable g, Num a) => (a -> a -> b) -> (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (a, f b)
jacobianWith2 g f as = transposeWith row t p
    where
        (p, t) = bindWith2 g' f as
        row x as' = (primal x, as')
        g' a ga = g a . tangent <$> ga
{-# INLINE jacobianWith2 #-}

grad :: (Traversable f, Num a) => (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> f a
grad f = bind (tangent . f)
{-# INLINE grad #-}


grad2 :: (Traversable f, Num a) => (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> (a, f a)
grad2 f as = (primal b, tangent <$> bs)
    where
        (b, bs) = bind2 f as
{-# INLINE grad2 #-}

gradWith :: (Traversable f, Num a) => (a -> a -> b) -> (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> f b
gradWith g f = bindWith g (tangent . f)
{-# INLINE gradWith #-}

gradWith2 :: (Traversable f, Num a) => (a -> a -> b) -> (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> (a, f b)
gradWith2 g f = bindWith2 g (tangent . f)
{-# INLINE gradWith2 #-}
