{-# LANGUAGE Rank2Types, TypeFamilies, DeriveDataTypeable, TemplateHaskell, UndecidableInstances, BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Internal.Forward
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only 
--
-- Unsafe and often partial combinators intended for internal usage.
--
-- Handle with care.
-----------------------------------------------------------------------------

module Numeric.AD.Internal.Forward
    ( Forward(..)
    , tangent
    , bundle
    , unbundle
    , apply
    , bind
    , bind2
    , transposeWith
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

