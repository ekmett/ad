{-# LANGUAGE Rank2Types, TypeFamilies, FlexibleContexts, UndecidableInstances, TemplateHaskell, DeriveDataTypeable, BangPatterns #-}
-- {-# OPTIONS_HADDOCK hide, prune #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Numeric.AD.Internal.Vectored
-- Copyright   : (c) Edward Kmett 2010
-- License     : BSD3
-- Maintainer  : ekmett@gmail.com
-- Stability   : experimental
-- Portability : GHC only
--
-- Bulk-Forward AD -- 
-- Assumes all instances of 'f' have the same number of elements.
--
-----------------------------------------------------------------------------

module Numeric.AD.Internal.Vectored
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
import Data.Vector (Vector)
import qualified Data.Vector as Vector

newtype Vectored a 
    = Lift a
    | a :| Vector (Grad a)
    deriving (Show)

fillWith :: Traversable f => (a -> c) -> f b -> Vector a -> f a
fillWith f as vs = snd $ mapAccumL (\vs' _ -> (Vector.tail vs, f (Vector.head vs))) vs
{-# INLINE fillWith #-}

-- can we turn this around so it'll fuse?
unbind :: Traversable f => f b -> AD Vectored a -> f a
unbind as vs = snd (unbind' as vs)
unbind as _ = 0 <$ as
{-# INLINE unbind #-}

unbind' :: Traversable f => f b -> AD Vectored a -> (a, f a)
unbind' as (AD (a :| vs)) = (a, fillWith primal as vs)
unbind' as _ = (a, 0 <$ as)
{-# INLINE unbind' #-}

d :: Traversable f => f b -> f b -> AD Vectored a -> f a
d fb (AD (_ :| da)) = fillWith primal fb da
d fz fb _           = fz
{-# INLINE d #-}

ds :: f a -> AD Vectored a -> (f :> a)
ds zero fb (AD (_ :| da)) = a :< fillWith (ds fb fb da
ds zero fb _              = 0 <$ fb
{-# INLINE ds #-}

d' :: f a -> AD (Grad f) a -> (a, f a)
d' fb (AD (a :| da)) = (a, fillWith primal fb da)
d' fb (AD (Lift a))  = (a, 0 <$ fb)
{-# INLINE d' #-}

bind :: (Traversable f, Num a) => f a -> (Int, f (AD Vectored a)
bind as = fmap (\a -> AD (a :| vs))) as
    where
        vs = Vector.replicate n (1 :| vs)
        size = foldMap (\_ -> 1) 
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
deriveLifted $ conT ''Grad `appT` varT ''f

