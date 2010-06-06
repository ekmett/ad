{-# LANGUAGE Rank2Types, BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Numeric.AD.Tower
-- Copyright   : (c) Edward Kmett 2010
-- License     : BSD3
-- Maintainer  : ekmett@gmail.com
-- Stability   : experimental
-- Portability : GHC only
--
-- Higher order derivatives via a \"dual number tower\".
--
-----------------------------------------------------------------------------

module Numeric.AD.Tower
    (
    -- * Taylor Series
      taylor
    , taylor0
    -- * Maclaurin Series
    , maclaurin
    , maclaurin0
    -- * Derivatives
    , diff
    , diff'
    , diffs
    , diffs0
    , diffsF
    , diffs0F
    -- * Monadic Combinators
    , diffsM
    , diffs0M
    -- * Exposed Types
    , Mode(..)
    , AD(..)
    ) where

import Control.Monad (liftM)
import Control.Applicative ((<$>))
import Numeric.AD.Classes
import Numeric.AD.Internal
import Numeric.AD.Internal.Tower

diffs :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> [a]
diffs f a = getADTower $ apply f a
{-# INLINE diffs #-}

diffs0 :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> [a]
diffs0 f a = zeroPad (diffs f a)
{-# INLINE diffs0 #-}

diffsF :: (Functor f, Num a) => (forall s. Mode s => AD s a -> f (AD s a)) -> a -> f [a]
diffsF f a = getADTower <$> apply f a
{-# INLINE diffsF #-}

diffs0F :: (Functor f, Num a) => (forall s. Mode s => AD s a -> f (AD s a)) -> a -> f [a]
diffs0F f a = (zeroPad . getADTower) <$> apply f a
{-# INLINE diffs0F #-}

diffsM :: (Monad m, Num a) => (forall s. Mode s => AD s a -> m (AD s a)) -> a -> m [a]
diffsM f a = getADTower `liftM` apply f a
{-# INLINE diffsM #-}

diffs0M :: (Monad m, Num a) => (forall s. Mode s => AD s a -> m (AD s a)) -> a -> m [a]
diffs0M f a = (zeroPad . getADTower) `liftM` apply f a
{-# INLINE diffs0M #-}

taylor :: Fractional a => (forall s. Mode s => AD s a -> AD s a) -> a -> a -> [a]
taylor f x dx = go 1 1 (diffs f x)
    where
        go !n !acc (a:as) = a * acc : go (n + 1) (acc * dx / n) as
        go _ _ [] = []

taylor0 :: Fractional a => (forall s. Mode s => AD s a -> AD s a) -> a -> a -> [a]
taylor0 f x dx = zeroPad (taylor f x dx)
{-# INLINE taylor0 #-}

maclaurin :: Fractional a => (forall s. Mode s => AD s a -> AD s a) -> a -> [a]
maclaurin f = taylor f 0
{-# INLINE maclaurin #-}

maclaurin0 :: Fractional a => (forall s. Mode s => AD s a -> AD s a) -> a -> [a]
maclaurin0 f = taylor0 f 0
{-# INLINE maclaurin0 #-}

diff :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> a
diff f a = d $ diffs f a
{-# INLINE diff #-}

diff' :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> (a, a)
diff' f a = d' $ diffs f a
{-# INLINE diff' #-}

