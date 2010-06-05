{-# LANGUAGE Rank2Types, TypeFamilies, DeriveDataTypeable, MultiParamTypeClasses, TemplateHaskell, FlexibleContexts, UndecidableInstances, BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Forward
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

module Numeric.AD.Forward
    ( AD(..)
    , Mode(..)
    , Forward(..)
    -- * Derivatives
    , diffUU
    , diff2UU
    , diffUF
    , diff2UF
    -- * Common access patterns
    , diff
    , diff2
    , jacobian
    , jacobian2
    , jacobianT
    , grad
    , grad2
    ) where

import Language.Haskell.TH
import Data.Typeable
import Data.Traversable (Traversable, mapAccumL)
import Data.Foldable (Foldable, toList)
import Data.Data
import Control.Applicative
import Numeric.AD.Classes
import Numeric.AD.Internal

data Forward a = Forward a a deriving (Show, Data, Typeable)

-- * Local combinators

tangent :: AD Forward a -> a
tangent (AD (Forward _ da)) = da
{-# INLINE tangent #-}

unbundle :: AD Forward a -> (a, a)
unbundle (AD (Forward a da)) = (a, da)
{-# INLINE unbundle #-}

bundle :: a -> a -> AD Forward a
bundle a da = AD (Forward a da)
{-# INLINE bundle #-}

apply :: Num a => (AD Forward a -> b) -> a -> b
apply f a = f (bundle a 1)
{-# INLINE apply #-}

instance Primal Forward where
    primal (Forward a _) = a

instance Lifted Forward => Mode Forward where
    lift a = Forward a 0
    Forward a da <+> Forward b db = Forward (a + b) (da + db)
    a *^ Forward b db = Forward (a * b) (a * db)
    Forward a da ^* b = Forward (a * b) (da * b)
    Forward a da ^/ b = Forward (a / b) (da / b)

instance Lifted Forward => Jacobian Forward where
    type D Forward = Id
    unary f (Id dadb) (Forward b db) = Forward (f b) (dadb * db)
    lift1 f df (Forward b db) = Forward (f b) (dadb * db)
        where 
            Id dadb = df (Id b)
    lift1_ f df (Forward b db) = Forward a da
        where 
            a = f b
            Id da = df (Id a) (Id b) ^* db

    binary f (Id dadb) (Id dadc) (Forward b db) (Forward c dc) = Forward (f b c) da
        where 
            da = dadb * db + dc * dadc
    lift2 f df (Forward b db) (Forward c dc) = Forward a da
        where 
            a = f b c
            (Id dadb, Id dadc) = df (Id b) (Id c) 
            da = dadb * db + dc * dadc
    lift2_ f df (Forward b db) (Forward c dc) = Forward a da
        where 
            a = f b c
            (Id dadb, Id dadc) = df (Id a) (Id b) (Id c)
            da = dadb * db + dc * dadc

deriveLifted $ conT ''Forward

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

bind :: (Traversable f, Num a) => (f (AD Forward a) -> b) -> f a -> f b
bind f as = snd $ mapAccumL outer 0 as
    where
        outer !i a = (i + 1, f $ snd $ mapAccumL (inner i) 0 as)
        inner !i !j a = (j + 1, bundle a $ if i == j then 1 else 0)

bind2 :: (Traversable f, Num a) => (f (AD Forward a) -> b) -> f a -> (b, f b)
bind2 f as = dropIx $ mapAccumL outer (0, b0) as 
    where
        outer (!i, _) a = let b = f $ snd $ mapAccumL (inner i) 0 as in ((i + 1, b), b)
        inner !i !j a = (j + 1, bundle a $ if i == j then 1 else 0)
        b0 = f (lift <$> as)
        dropIx ((_,b),bs) = (b,bs)

-- we can't transpose arbitrary traversables, since we can't construct one out of whole cloth, and the outer
-- traversable could be empty. So instead we use one as a 'skeleton'
transposeWith :: (Functor f, Foldable f, Traversable g) => (b -> f a -> c) -> f (g a) -> g b -> g c
transposeWith f as = snd . mapAccumL go xss0
    where 
        go xss b = (tail <$> xss, f b (head <$> xss))
        xss0 = toList <$> as

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


{-
jacobian2 :: (Traversable f, Traversable g, Num a) => (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (a, f a)
jacobian2 f as = transposeWith (\x fa -> (unprobe x, tangent <$> fa)) t p 
    where
        t = bind f as 
        p = f (probe <$> as)
{-# INLINE jacobian2 #-}
-}
