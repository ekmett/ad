{-# LANGUAGE Rank2Types, TypeFamilies, FlexibleContexts, UndecidableInstances, TemplateHaskell, DeriveDataTypeable, BangPatterns #-}
-- {-# OPTIONS_HADDOCK hide, prune #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Numeric.AD.Internal.Grad
-- Copyright   : (c) Edward Kmett 2010
-- License     : BSD3
-- Maintainer  : ekmett@gmail.com
-- Stability   : experimental
-- Portability : GHC only
--
-- Bulk-Forward AD
--
-- Assumes all instances of 'f' have the same number of elements.
--
-----------------------------------------------------------------------------

module Numeric.AD.Internal.Grad
    ( Grad(..)
    , ds
    , ds'
    , zeroes
    , ones
    , vars
    , apply
    ) where

import Language.Haskell.TH
import Data.Typeable
import Data.Traversable (Traversable, mapAccumL)
import Data.Foldable (Foldable, toList)
import Data.Data
import Control.Applicative
import Numeric.AD.Internal.Types
import Numeric.AD.Internal.Combinators
import Numeric.AD.Internal.Classes
import Numeric.AD.Internal.Identity

newtype Grad f a 
    = Lift a
    | Grad a (f a) 
    deriving (Show)

ds :: f a -> AD (Grad f) a -> f a
ds _    (AD (Grad _ da)) = da
ds zero _                = zero
{-# INLINE tangent #-}

ds' :: f a -> AD (Grad f) a -> (a, f a)
ds' _    (AD (Grad a da)) = (a, da)
ds' zero (AD (Lift a))    = (a, zero)
{-# INLINE ds' #-}

-- Bind variables and count inputs
vars :: (Traversable f, Num a) => f a -> f (AD (Grad f) a)
vars as = snd $ mapAccumL outer (0 :: Int) as
    where
        outer !i a = (i + 1, Grad a $ snd $ mapAccumL (inner i) 0 as)
        inner !i !j a = (j + 1, if i == j then 1 else 0)
{-# INLINE vars #-}

apply :: (Functor f, Num a) => (f (AD (Grad f) a) -> b) -> f a -> b
apply f as = f (vars as)
{-# INLINE apply #-}

instance Primal (Grad f) where
    primal (Lift a) = a
    primal (Grad a _) = a

instance (Traversable f, Lifted (Grad f)) => Mode (Grad f) where
    lift = Lift a
    Lift a <+> Lift b = Lift (a + b)
    Lift a <+> Grad b db = Grad (a + b) db
    Grad a da <+> Lift b = Grad (a + b) db
    Grad a da <+> Grad b db = Grad (a + b) $ zipWithT (+) da db -- relies on shape
    a *^ Lift b    = Lift (a * b)
    a *^ Grad b db = Grad (a * b) (fmap (a*) db)
    Lift a    ^* b = Lift (a * b)
    Grad a da ^* b = Grad (a * b) (fmap (*b) da)
    Lift a    ^/ b = Lift (a / b)
    Grad a da ^/ b = Grad (a / b) (fmap (/b) da)

instance (Traversable f, Lifted (Grad f)) => Jacobian (Grad f) where
    type D (Grad f) = Id
    unary f _         (Lift b)    = Lift (f b)
    unary f (Id dadb) (Grad b db) = Grad (f b) (fmap (dadb *) db)
    
    lift1 f _  (Lift b)    = Lift (f b)
    lift1 f df (Grad b db) = Grad (f b) (fmap (dadb *) db)
        where 
            Id dadb = df (Id b)

    lift1_ f _  (Lift b)    = Lift (f b)
    lift1_ f df (Grad b db) = Grad a (fmap (dadb *) da)
        where
            a = f b
            Id dadb = df (Id a) (Id b) ^* db

    binary f _         _         (Lift b)    (Lift c)    = Lift (f b c)
    binary f _         (Id dadc) (Lift b)    (Grad c dc) = Grad (f b c) (fmap (* dadc) dc)
    binary f (Id dadb) _         (Grad b db) (Lift c)    = Grad (f b c) (fmap (dadb *) db)
    binary f (Id dadb) (Id dadc) (Grad b db) (Grad c dc) = Grad (f b c) (zipWithT (\dbi dci -> dadb * dbi + dci * dadc) db dc)

    lift2 f _  (Lift b)    (Lift c)    = Lift (f b c)
    lift2 f df (Lift b)    (Grad c dc) = Grad (f b c) (fmap (*dadc) dc) where (_, Id dadc) = df (Id b) (Id c)
    lift2 f df (Grad b db) (Lift c)    = Grad (f b c) (fmap (dadb*) db) where (Id dadb, _) = df (Id b) (Id c)
    lift2 f df (Grad b dc) (Grad c dc) = Grad a da
        where
            a = f b c
            (Id dadb, Id dadc) = df (Id b) (Id c)
            da = zipWithT (\dbi dci -> dadb * dbi + dci * dadc) db dc
            
    lift2_ f _  (Lift b) (Lift c)       = Lift (f b c)
    lift2_ f df (Lift b)    (Grad c dc) = Grad (f b c) (fmap (*dadc) dc) where (_, Id dadc) = df (Id a) (Id b) (Id c)
    lift2_ f df (Grad b db) (Lift c)    = Grad (f b c) (fmap (dadb*) db) where (Id dadb, _) = df (Id a) (Id b) (Id c)
    lift2_ f df (Grad b dc) (Grad c dc) = Grad a       (zipWithT (\dbi dci -> dadb * dbi + dci * dadc) db dc)
        where
            a = f b c
            (Id dadb, Id dadc) = df (Id a) (Id b) (Id c)

-- we need to be able to add context here!
let f = varT (mkName "f") in
    deriveLifted (classP ''Traversable [f]:) (conT ''Grad `appT` f)
