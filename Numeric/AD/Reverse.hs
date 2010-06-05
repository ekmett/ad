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
    -- * Jacobian
    , jacobian
    , jacobian2
    -- * Derivatives
    , diffUU
    , diff2UU
    , diffFU
    , diff2FU
    , diffUF
    , diff2UF
    -- * Synonyms
    , diff
    , diff2
    -- * Exposed Types
    , AD(..)
    , Mode(..)
    ) where

import Control.Applicative ((<$>))
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

diffUU :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> a
diffUU f a = derivative $ f (var a 0)
{-# INLINE diffUU #-}

diffUF :: (Functor f, Num a) => (forall s. Mode s => AD s a -> f (AD s a)) -> a -> f a
diffUF f a = derivative <$> f (var a 0)
{-# INLINE diffUF #-}

-- | The 'diff2UU' function calculates the value and derivative, as a
-- pair, of a scalar-to-scalar function.
diff2UU :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> (a, a)
diff2UU f a = derivative2 $ f (var a 0)
{-# INLINE diff2UU #-}

diff2UF :: (Functor f, Num a) => (forall s. Mode s => AD s a -> f (AD s a)) -> a -> f (a, a)
diff2UF f a = derivative2 <$> f (var a 0)
{-# INLINE diff2UF #-}

diffFU :: (Traversable f, Num a) => (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> f a
diffFU f as = unbind vs $ partialArray bds (f vs)
    where (vs, bds) = bind as
{-# INLINE diffFU #-}

diff2FU :: (Traversable f, Num a) => (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> (a, f a)
diff2FU f as = (primal result, unbind vs $ partialArray bds result)
    where (vs, bds) = bind as
          result = f vs
{-# INLINE diff2FU #-}

-- | The 'diff' function is a synonym for 'diffUU'.
diff :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> a
diff = diffUU 
{-# INLINE diff #-}

-- | The 'diff2' function is a synonym for 'diff2UU'.
diff2 :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> (a, a)
diff2 = diff2UU
{-# INLINE diff2 #-}

