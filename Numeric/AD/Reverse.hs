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
    , grad2
    , gradWith
    , gradWith2
    -- * Jacobian
    , jacobian
    , jacobian2
    , jacobianWith
    , jacobianWith2
    -- * Derivatives
    , diffUU
    , diff2UU
    , diffUF
    , diff2UF
    -- * Synonyms
    , diff
    , diff2
    -- * Monadic Combinators
    , diffUM
    , diff2UM
    , gradM
    , grad2M
    , gradWithM
    , gradWith2M
    -- * Exposed Types
    , AD(..)
    , Mode(..)
    ) where

import Control.Monad (liftM)
import Control.Applicative (WrappedMonad(..),(<$>))
import Data.Traversable (Traversable)

import Numeric.AD.Classes
import Numeric.AD.Internal
import Numeric.AD.Internal.Reverse

-- | The 'grad' function calculates the gradient of a non-scalar-to-scalar function with 'Reverse' AD in a single pass.
grad :: (Traversable f, Num a) => (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> f a
grad f as = unbind vs (partialArray bds $ f vs)
    where (vs,bds) = bind as
{-# INLINE grad #-}

-- | The 'grad2' function calculates the result and gradient of a non-scalar-to-scalar function with 'Reverse' AD in a single pass.
grad2 :: (Traversable f, Num a) => (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> (a, f a)
grad2 f as = (primal r, unbind vs $ partialArray bds r)
    where (vs, bds) = bind as
          r = f vs
{-# INLINE grad2 #-}

-- | @'grad' g f@ function calculates the gradient of a non-scalar-to-scalar function @f@ with reverse-mode AD in a single pass.
-- The gradient is combined element-wise with the argument using the function @g@.
--
-- > grad == gradWith (\_ dx -> dx)
-- > id == gradWith const
gradWith :: (Traversable f, Num a) => (a -> a -> b) -> (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> f b
gradWith g f as = unbindWith g vs (partialArray bds $ f vs)
    where (vs,bds) = bind as
{-# INLINE gradWith #-}

-- | @'grad2' g f@ calculates the result and gradient of a non-scalar-to-scalar function @f@ with 'Reverse' AD in a single pass
-- the gradient is combined element-wise with the argument using the function @g@.
--
-- > grad2 == gradWith2 (\_ dx -> dx)
gradWith2 :: (Traversable f, Num a) => (a -> a -> b) -> (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> (a, f b)
gradWith2 g f as = (primal r, unbindWith g vs $ partialArray bds r)
    where (vs, bds) = bind as
          r = f vs
{-# INLINE gradWith2 #-}

-- | The 'jacobian' function calculates the jacobian of a non-scalar-to-non-scalar function with reverse AD lazily in @m@ passes for @m@ outputs.
jacobian :: (Traversable f, Functor g, Num a) => (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (f a)
jacobian f as = unbind vs . partialArray bds <$> f vs where
    (vs, bds) = bind as
{-# INLINE jacobian #-}

-- | The 'jacobian2' function calculates both the result and the Jacobian of a nonscalar-to-nonscalar function, using @m@ invocations of reverse AD,
-- where @m@ is the output dimensionality. Applying @fmap snd@ to the result will recover the result of 'jacobian'
jacobian2 :: (Traversable f, Functor g, Num a) => (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (a, f a)
jacobian2 f as = row <$> f vs where
    (vs, bds) = bind as
    row a = (primal a, unbind vs (partialArray bds a))
{-# INLINE jacobian2 #-}

-- | 'jacobianWith g f' calculates the jacobian of a non-scalar-to-non-scalar function @f@ with reverse AD lazily in @m@ passes for @m@ outputs.
--
-- Instead of returning the Jacobian matrix, the elements of the matrix are combined with the input using the @g@.
--
-- > jacobian == jacobianWith (\_ dx -> dx)
-- > jacobianWith const == (\f x -> const x <$> f x)
--
jacobianWith :: (Traversable f, Functor g, Num a) => (a -> a -> b) -> (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (f b)
jacobianWith g f as = unbindWith g vs . partialArray bds <$> f vs where
    (vs, bds) = bind as
{-# INLINE jacobianWith #-}

-- | 'jacobianWith2 g f' calculates both the result and the Jacobian of a nonscalar-to-nonscalar function @f@, using @m@ invocations of reverse AD,
-- where @m@ is the output dimensionality. Applying @fmap snd@ to the result will recover the result of 'jacobianWith'
--
-- Instead of returning the Jacobian matrix, the elements of the matrix are combined with the input using the @g@.
--
-- > jacobian2 == jacobianWith2 (\_ dx -> dx)
--
jacobianWith2 :: (Traversable f, Functor g, Num a) => (a -> a -> b) -> (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (a, f b)
jacobianWith2 g f as = row <$> f vs where
    (vs, bds) = bind as
    row a = (primal a, unbindWith g vs (partialArray bds a))
{-# INLINE jacobianWith2 #-}

diffUU :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> a
diffUU f a = derivative $ f (var a 0)
{-# INLINE diffUU #-}

-- | The 'diff2UU' function calculates the value and derivative, as a
-- pair, of a scalar-to-scalar function.
diff2UU :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> (a, a)
diff2UU f a = derivative2 $ f (var a 0)
{-# INLINE diff2UU #-}

diffUF :: (Functor f, Num a) => (forall s. Mode s => AD s a -> f (AD s a)) -> a -> f a
diffUF f a = derivative <$> f (var a 0)
{-# INLINE diffUF #-}

diff2UF :: (Functor f, Num a) => (forall s. Mode s => AD s a -> f (AD s a)) -> a -> f (a, a)
diff2UF f a = derivative2 <$> f (var a 0)
{-# INLINE diff2UF #-}

diffUM :: (Monad m, Num a) => (forall s. Mode s => AD s a -> m (AD s a)) -> a -> m a
diffUM f a = liftM derivative $ f (var a 0)
{-# INLINE diffUM #-}

diff2UM :: (Monad m, Num a) => (forall s. Mode s => AD s a -> m (AD s a)) -> a -> m (a, a)
diff2UM f a = liftM derivative2 $ f (var a 0)
{-# INLINE diff2UM #-}

-- | The 'diff' function is a synonym for 'diffUU'.
diff :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> a
diff = diffUU
{-# INLINE diff #-}

-- | The 'diff2' function is a synonym for 'diff2UU'.
diff2 :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> (a, a)
diff2 = diff2UU
{-# INLINE diff2 #-}

gradM :: (Traversable f, Monad m, Num a) => (forall s. Mode s => f (AD s a) -> m (AD s a)) -> f a -> m (f a)
gradM f = unwrapMonad . jacobian (WrapMonad . f)
{-# INLINE gradM #-}

grad2M :: (Traversable f, Monad m, Num a) => (forall s. Mode s => f (AD s a) -> m (AD s a)) -> f a -> m (a, f a)
grad2M f = unwrapMonad . jacobian2 (WrapMonad . f)
{-# INLINE grad2M #-}

gradWithM :: (Traversable f, Monad m, Num a) => (a -> a -> b) -> (forall s. Mode s => f (AD s a) -> m (AD s a)) -> f a -> m (f b)
gradWithM g f = unwrapMonad . jacobianWith g (WrapMonad . f)

gradWith2M :: (Traversable f, Monad m, Num a) => (a -> a -> b) -> (forall s. Mode s => f (AD s a) -> m (AD s a)) -> f a -> m (a, f b)
gradWith2M g f = unwrapMonad . jacobianWith2 g (WrapMonad . f)

