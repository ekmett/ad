{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Numeric.AD.Tower
-- Copyright   : (c) Edward Kmett 2010
-- License     : BSD3
-- Maintainer  : ekmett@gmail.com
-- Stability   : experimental
-- Portability : GHC only 
--
-- Mixed-Mode Automatic Differentiation.
-- 
-- For reverse mode AD we use 'System.Mem.StableName.StableName' to recover sharing information from 
-- the tape to avoid combinatorial explosion, and thus run asymptotically faster
-- than it could without such sharing information, but the use of side-effects
-- contained herein is benign.
--
-----------------------------------------------------------------------------

module Numeric.AD.Tower
    ( 
    -- * multiple derivatives
      diffsUU, diffs0UU
    , diffsUF, diffs0UF 
    -- * single derivatives
    , diffUU, diff2UU
    -- * common access patterns
    , diffs, diffs0
    , diff, diff2
    -- * taylor series
    , taylor, taylor0
    -- * internals
    , Mode(..)
    , AD(..)
    , Tower(..)
    ) where

import Control.Applicative
import Numeric.AD.Classes
import Numeric.AD.Internal
import Numeric.AD.Tower.Internal
import Data.List (mapAccumL)

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
taylor f x dx = snd $ 
    mapAccumL (\a x -> diag (a + x)) 0 $
    zipWith3 (\x y z -> x * y * z)    
             (diffsUU f x)
             recipFactorials
             (powers x)
    where
        powers x = iterate (*x) 1
        recipFactorials = snd $ mapAccumL (\a i -> (a / fromIntegral i, a)) 1 [1..]
        diag x = (x, x)

taylor0 :: Fractional a => (forall s. Mode s => AD s a -> AD s a) -> a -> a -> [a]
taylor0 f x dx = zeroPad (taylor f x dx)
{-# INLINE taylor0 #-}

-- | This is an inefficient 'Mode'. Use 'Numeric.AD.Forward.diffUU' instead.
diffUU :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> a
diffUU f a = d $ diffs f a 
{-# INLINE diffUU #-}

-- | This is an inefficient 'Mode'. Use 'Numeric.AD.Forward.diff2UU' instead.
diff2UU :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> (a, a)
diff2UU f a = d2 $ diffs f a 
{-# INLINE diff2UU #-}

diff :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> a
diff = diffUU
{-# INLINE diff #-}

diff2 :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> (a, a)
diff2 = diff2UU
{-# INLINE diff2 #-}
