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
    -- * Jacobian
    , jacobian
    , jacobian2
    , jacobianT
    -- * Derivatives
    , diffUU
    , diff2UU
    , diffUF
    , diff2UF
    -- * Synonyms
    , diff
    , diff2
    -- * Exposed Types
    , AD(..)
    , Mode(..)
    ) where

import Data.Traversable (Traversable)
import Control.Applicative
import Numeric.AD.Classes
import Numeric.AD.Internal
import Numeric.AD.Internal.Forward

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

-- A fast, simple transposed forward jacobian
jacobianT :: (Traversable f, Functor g, Num a) => (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> f (g a)
jacobianT f = bind (fmap tangent . f)
-- jacobianT f as = fmap tangent <$> bind f as
{-# INLINE jacobianT #-}

jacobian :: (Traversable f, Traversable g, Num a) => (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (f a)
jacobian f as = transposeWith (const id) t p
    where 
        (p, t) = bind2 (fmap tangent . f) as 
{-# INLINE jacobian #-}

jacobian2 :: (Traversable f, Traversable g, Num a) => (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (a, f a)
jacobian2 f as = transposeWith row t p
    where 
        (p, t) = bind2 f as 
        row x as = (primal x, tangent <$> as) 
{-# INLINE jacobian2 #-}

grad :: (Traversable f, Num a) => (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> f a
grad f = bind (tangent . f)
{-# INLINE grad #-}

grad2 :: (Traversable f, Num a) => (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> (a, f a)
grad2 f as = (primal b, tangent <$> bs)
    where 
        (b, bs) = bind2 f as
{-# INLINE grad2 #-}
