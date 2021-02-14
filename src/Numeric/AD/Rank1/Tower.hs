{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   : (c) Edward Kmett 2010-2021
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
  , auto
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
  , du      -- directional derivative of (f a -> a)
  , du'     -- answer and directional derivative of (f a -> a)
  , dus     -- answer and all directional derivatives of (f a -> a)
  , dus0    -- answer and all zero padded directional derivatives of (f a -> a)
  , duF     -- directional derivative of (f a -> g a)
  , duF'    -- answer and directional derivative of (f a -> g a)
  , dusF    -- answer and all directional derivatives of (f a -> g a)
  , dus0F   -- answer and all zero padded directional derivatives of (f a -> g a)
  ) where

import Numeric.AD.Internal.Tower
import Numeric.AD.Mode

-- | Compute the answer and all derivatives of a function @(a -> a)@
diffs
  :: Num a
  => (Tower a -> Tower a)
  -> a
  -> [a]
diffs f a = getADTower $ apply f a
{-# INLINE diffs #-}

-- | Compute the zero-padded derivatives of a function @(a -> a)@
diffs0
  :: Num a
  => (Tower a -> Tower a)
  -> a
  -> [a]
diffs0 f a = zeroPad (diffs f a)
{-# INLINE diffs0 #-}

-- | Compute the answer and all derivatives of a function @(a -> f a)@
diffsF
  :: (Functor f, Num a)
  => (Tower a -> f (Tower a))
  -> a
  -> f [a]
diffsF f a = getADTower <$> apply f a
{-# INLINE diffsF #-}

-- | Compute the zero-padded derivatives of a function @(a -> f a)@
diffs0F
  :: (Functor f, Num a)
  => (Tower a -> f (Tower a))
  -> a
  -> f [a]
diffs0F f a = zeroPad . getADTower <$> apply f a
{-# INLINE diffs0F #-}

-- | @taylor f x@ compute the Taylor series of @f@ around @x@.
taylor
  :: Fractional a
  => (Tower a -> Tower a)
  -> a
  -> a
  -> [a]
taylor f x dx = go 1 1 (diffs f x) where
  go !n !acc (a:as) = a * acc : go (n + 1) (acc * dx / n) as
  go _ _ [] = []

-- | @taylor0 f x@ compute the Taylor series of @f@ around @x@, zero-padded.
taylor0
  :: Fractional a
  => (Tower a -> Tower a)
  -> a
  -> a
  -> [a]
taylor0 f x dx = zeroPad (taylor f x dx)
{-# INLINE taylor0 #-}

-- | @maclaurin f@ compute the Maclaurin series of @f@
maclaurin
  :: Fractional a
  => (Tower a -> Tower a)
  -> a
  -> [a]
maclaurin f = taylor f 0
{-# INLINE maclaurin #-}

-- | @maclaurin f@ compute the Maclaurin series of @f@, zero-padded
maclaurin0
  :: Fractional a
  => (Tower a -> Tower a)
  -> a
  -> [a]
maclaurin0 f = taylor0 f 0
{-# INLINE maclaurin0 #-}

-- | Compute the first derivative of a function @(a -> a)@
diff
  :: Num a
  => (Tower a -> Tower a)
  -> a
  -> a
diff f = d . diffs f
{-# INLINE diff #-}

-- | Compute the answer and first derivative of a function @(a -> a)@
diff'
  :: Num a
  => (Tower a -> Tower a)
  -> a
  -> (a, a)
diff' f = d' . diffs f
{-# INLINE diff' #-}

-- | Compute a directional derivative of a function @(f a -> a)@
du
  :: (Functor f, Num a)
  => (f (Tower a) -> Tower a)
  -> f (a, a) -> a
du f = d . getADTower . f . fmap withD
{-# INLINE du #-}

-- | Compute the answer and a directional derivative of a function @(f a -> a)@
du'
  :: (Functor f, Num a)
  => (f (Tower a) -> Tower a)
  -> f (a, a)
  -> (a, a)
du' f = d' . getADTower . f . fmap withD
{-# INLINE du' #-}

-- | Compute a directional derivative of a function @(f a -> g a)@
duF
  :: (Functor f, Functor g, Num a)
  => (f (Tower a) -> g (Tower a))
  -> f (a, a)
  -> g a
duF f = fmap (d . getADTower) . f . fmap withD
{-# INLINE duF #-}

-- | Compute the answer and a directional derivative of a function @(f a -> g a)@
duF'
  :: (Functor f, Functor g, Num a)
  => (f (Tower a) -> g (Tower a))
  -> f (a, a)
  -> g (a, a)
duF' f = fmap (d' . getADTower) . f . fmap withD
{-# INLINE duF' #-}

-- | Given a function @(f a -> a)@, and a tower of derivatives, compute the corresponding directional derivatives.
dus
  :: (Functor f, Num a)
  => (f (Tower a) -> Tower a)
  -> f [a]
  -> [a]
dus f = getADTower . f . fmap tower
{-# INLINE dus #-}

-- | Given a function @(f a -> a)@, and a tower of derivatives, compute the corresponding directional derivatives, zero-padded
dus0
  :: (Functor f, Num a)
  => (f (Tower a) -> Tower a)
  -> f [a]
  -> [a]
dus0 f = zeroPad . getADTower . f . fmap tower
{-# INLINE dus0 #-}

-- | Given a function @(f a -> g a)@, and a tower of derivatives, compute the corresponding directional derivatives
dusF
  :: (Functor f, Functor g, Num a)
  => (f (Tower a) -> g (Tower a))
  -> f [a]
  -> g [a]
dusF f = fmap getADTower . f . fmap tower
{-# INLINE dusF #-}

-- | Given a function @(f a -> g a)@, and a tower of derivatives, compute the corresponding directional derivatives, zero-padded
dus0F
  :: (Functor f, Functor g, Num a)
  => (f (Tower a) -> g (Tower a))
  -> f [a]
  -> g [a]
dus0F f = fmap getADTower . f . fmap tower
{-# INLINE dus0F #-}
