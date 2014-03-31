{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   : (c) Edward Kmett 2010-2014
-- License     : BSD3
-- Maintainer  : ekmett@gmail.com
-- Stability   : experimental
-- Portability : GHC only
--
-- Higher order derivatives via a \"dual number tower\".
--
-----------------------------------------------------------------------------

module Numeric.AD.Rank1.Tower
  ( Tower
  -- * Taylor Series
  , taylor
  , taylor0
  -- * Maclaurin Series
  , maclaurin
  , maclaurin0
  -- * Derivatives
  , diff    -- first derivative of (a -> a)
  , diff'   -- answer and first derivative of (a -> a)
  , diffs   -- answer and all derivatives of (a -> a)
  , diffs0  -- zero padded derivatives of (a -> a)
  , diffsF  -- answer and all derivatives of (a -> f a)
  , diffs0F -- zero padded derivatives of (a -> f a)
  -- * Directional Derivatives
  , du      -- directional derivative of (a -> a)
  , du'     -- answer and directional derivative of (a -> a)
  , dus     -- answer and all directional derivatives of (a -> a)
  , dus0    -- answer and all zero padded directional derivatives of (a -> a)
  , duF     -- directional derivative of (a -> f a)
  , duF'    -- answer and directional derivative of (a -> f a)
  , dusF    -- answer and all directional derivatives of (a -> f a)
  , dus0F   -- answer and all zero padded directional derivatives of (a -> a)
  ) where

import Control.Applicative ((<$>))
import Numeric.AD.Internal.Tower

diffs :: Num a => (Tower a -> Tower a) -> a -> [a]
diffs f a = getADTower $ apply f a
{-# INLINE diffs #-}

diffs0 :: Num a => (Tower a -> Tower a) -> a -> [a]
diffs0 f a = zeroPad (diffs f a)
{-# INLINE diffs0 #-}

diffsF :: (Functor f, Num a) => (Tower a -> f (Tower a)) -> a -> f [a]
diffsF f a = getADTower <$> apply f a
{-# INLINE diffsF #-}

diffs0F :: (Functor f, Num a) => (Tower a -> f (Tower a)) -> a -> f [a]
diffs0F f a = (zeroPad . getADTower) <$> apply f a
{-# INLINE diffs0F #-}

taylor :: Fractional a => (Tower a -> Tower a) -> a -> a -> [a]
taylor f x dx = go 1 1 (diffs f x) where
  go !n !acc (a:as) = a * acc : go (n + 1) (acc * dx / n) as
  go _ _ [] = []

taylor0 :: Fractional a => (Tower a -> Tower a) -> a -> a -> [a]
taylor0 f x dx = zeroPad (taylor f x dx)
{-# INLINE taylor0 #-}

maclaurin :: Fractional a => (Tower a -> Tower a) -> a -> [a]
maclaurin f = taylor f 0
{-# INLINE maclaurin #-}

maclaurin0 :: Fractional a => (Tower a -> Tower a) -> a -> [a]
maclaurin0 f = taylor0 f 0
{-# INLINE maclaurin0 #-}

diff :: Num a => (Tower a -> Tower a) -> a -> a
diff f = d . diffs f
{-# INLINE diff #-}

diff' :: Num a => (Tower a -> Tower a) -> a -> (a, a)
diff' f = d' . diffs f
{-# INLINE diff' #-}

du :: (Functor f, Num a) => (f (Tower a) -> Tower a) -> f (a, a) -> a
du f = d . getADTower . f . fmap withD
{-# INLINE du #-}

du' :: (Functor f, Num a) => (f (Tower a) -> Tower a) -> f (a, a) -> (a, a)
du' f = d' . getADTower . f . fmap withD
{-# INLINE du' #-}

duF :: (Functor f, Functor g, Num a) => (f (Tower a) -> g (Tower a)) -> f (a, a) -> g a
duF f = fmap (d . getADTower) . f . fmap withD
{-# INLINE duF #-}

duF' :: (Functor f, Functor g, Num a) => (f (Tower a) -> g (Tower a)) -> f (a, a) -> g (a, a)
duF' f = fmap (d' . getADTower) . f . fmap withD
{-# INLINE duF' #-}

dus :: (Functor f, Num a) => (f (Tower a) -> Tower a) -> f [a] -> [a]
dus f = getADTower . f . fmap tower
{-# INLINE dus #-}

dus0 :: (Functor f, Num a) => (f (Tower a) -> Tower a) -> f [a] -> [a]
dus0 f = zeroPad . getADTower . f . fmap tower
{-# INLINE dus0 #-}

dusF :: (Functor f, Functor g, Num a) => (f (Tower a) -> g (Tower a)) -> f [a] -> g [a]
dusF f = fmap getADTower . f . fmap tower
{-# INLINE dusF #-}

dus0F :: (Functor f, Functor g, Num a) => (f (Tower a) -> g (Tower a)) -> f [a] -> g [a]
dus0F f = fmap getADTower . f . fmap tower
{-# INLINE dus0F #-}
