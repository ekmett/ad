{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
-- {-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Internal.Vector
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------

module Numeric.AD.Internal.Vector
    ( WrappedVector(..)
    , wrap
    , unwrap
    ) where

import Prelude hiding ((++))
import Control.Applicative
import Control.Monad
import Data.Data
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..))
import Data.Typeable ()
import Data.Monoid
import qualified Data.Vector as Vector
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Fusion.Stream as Stream
import Data.Vector.Fusion.Stream.Size
import Data.Vector (Vector, (++))
import Numeric.AD.Internal.Classes (Iso(..))

newtype WrappedVector a = WrappedVector (Vector a) 
    deriving (Iso (Vector a), Data, Typeable)

instance Functor WrappedVector where
    fmap f (WrappedVector v) = wrap (Vector.map f v)
    {-# INLINE fmap #-}

instance Monad WrappedVector where
    return = wrap . Vector.singleton
    {-# INLINE return #-}
    WrappedVector v >>= f = wrap $ Vector.concatMap (unwrap . f) v 
    {-# INLINE (>>=) #-}
    fail _ = wrap Vector.empty
    {-# INLINE fail #-}

instance MonadPlus WrappedVector where
    mzero = mempty
    {-# INLINE mzero #-}
    mplus = mappend
    {-# INLINE mplus #-}

instance Applicative WrappedVector where
    pure = wrap . Vector.singleton
    {-# INLINE pure #-}
    WrappedVector fs <*> WrappedVector as = 
        wrap $ G.unstream $ Stream.sized results (Exact n)
        where 
            n = Vector.length fs * Vector.length as
            results = Stream.concatMap body $ G.stream fs
            body f = Stream.map f $ G.stream as
    {-# INLINE (<*>) #-}

instance Alternative WrappedVector where
    (<|>) = mappend
    {-# INLINE (<|>) #-}
    empty = mempty
    {-# INLINE empty #-}

instance Monoid (WrappedVector a) where
    mempty = wrap Vector.empty
    {-# INLINE mempty #-}
    mappend u v = wrap (unwrap u ++ unwrap v)
    {-# INLINE mappend #-}

instance Foldable WrappedVector where
    foldl f z = Vector.foldl f z . unwrap 
    {-# INLINE foldl #-}
    foldr f z = Vector.foldr f z . unwrap 
    {-# INLINE foldr #-}
    foldl1 f = Vector.foldl1 f . unwrap 
    {-# INLINE foldl1 #-}
    foldr1 f = Vector.foldr1 f . unwrap
    {-# INLINE foldr1 #-}

instance Traversable WrappedVector where
    traverse f (WrappedVector v)
        = wrap . Vector.fromListN (Vector.length v) <$> traverse f (Vector.toList v)
    {-# INLINE traverse #-}

wrap :: Vector a -> WrappedVector a
wrap = WrappedVector
{-# INLINE wrap #-}

unwrap :: WrappedVector a -> Vector a
unwrap (WrappedVector v) = v
{-# INLINE unwrap #-}
