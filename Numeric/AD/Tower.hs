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
      taylor, taylor0
    , maclaurin, maclaurin0
    -- * Derivatives
    , diffUU
    , diff2UU
    , diffsUU
    , diffs0UU
    , diffsUF
    , diffs0UF 
    -- * Synonyms
    , diffs, diffs0
    , diff, diff2
    -- * Exposed Types
    , Mode(..)
    , AD(..)
    ) where

-- TODO: argminNaiveGradient

import Control.Applicative
import Numeric.AD.Classes
import Numeric.AD.Internal
import Numeric.AD.Internal.Tower

diffsUU :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> [a]
diffsUU f a = getADTower $ apply f a 
{-# INLINE diffsUU #-}

diffs0UU :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> [a]
diffs0UU f a = zeroPad (diffsUU f a)
{-# INLINE diffs0UU #-}

diffs0UF :: (Functor f, Num a) => (forall s. Mode s => AD s a -> f (AD s a)) -> a -> f [a]
diffs0UF f a = (zeroPad . getADTower) <$> apply f a
{-# INLINE diffs0UF #-}

diffsUF :: (Functor f, Num a) => (forall s. Mode s => AD s a -> f (AD s a)) -> a -> f [a]
diffsUF f a = getADTower <$> apply f a
{-# INLINE diffsUF #-}

diffs :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> [a]
diffs = diffsUU
{-# INLINE diffs #-}

diffs0 :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> [a]
diffs0 = diffs0UU
{-# INLINE diffs0 #-}

taylor :: Fractional a => (forall s. Mode s => AD s a -> AD s a) -> a -> a -> [a]
taylor f x dx = go 1 1 (diffs f x)
    where
        go !n !acc (d:ds) = d * acc : go (n + 1) (acc * dx / n) ds
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

diffUU :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> a
diffUU f a = d $ diffs f a 
{-# INLINE diffUU #-}

diff2UU :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> (a, a)
diff2UU f a = d2 $ diffs f a 
{-# INLINE diff2UU #-}

diff :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> a
diff = diffUU
{-# INLINE diff #-}

diff2 :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> (a, a)
diff2 = diff2UU
{-# INLINE diff2 #-}
