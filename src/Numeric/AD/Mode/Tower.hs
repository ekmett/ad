{-# LANGUAGE Rank2Types #-}
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
module Numeric.AD.Mode.Tower
  ( AD
  , Tower
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
  , du      -- directional derivative of (a -> a)
  , du'     -- answer and directional derivative of (a -> a)
  , dus     -- answer and all directional derivatives of (a -> a)
  , dus0    -- answer and all zero padded directional derivatives of (a -> a)
  , duF     -- directional derivative of (a -> f a)
  , duF'    -- answer and directional derivative of (a -> f a)
  , dusF    -- answer and all directional derivatives of (a -> f a)
  , dus0F   -- answer and all zero padded directional derivatives of (a -> a)
  ) where

import qualified Numeric.AD.Rank1.Tower as Rank1
import Numeric.AD.Internal.Tower (Tower)
import Numeric.AD.Internal.Type (AD(..))
import Numeric.AD.Mode

diffs :: Num a => (forall s. AD s (Tower a) -> AD s (Tower a)) -> a -> [a]
diffs f = Rank1.diffs (runAD.f.AD)
{-# INLINE diffs #-}

diffs0 :: Num a => (forall s. AD s (Tower a) -> AD s (Tower a)) -> a -> [a]
diffs0 f = Rank1.diffs0 (runAD.f.AD)
{-# INLINE diffs0 #-}

diffsF :: (Functor f, Num a) => (forall s. AD s (Tower a) -> f (AD s (Tower a))) -> a -> f [a]
diffsF f = Rank1.diffsF (fmap runAD.f.AD)
{-# INLINE diffsF #-}

diffs0F :: (Functor f, Num a) => (forall s. AD s (Tower a) -> f (AD s (Tower a))) -> a -> f [a]
diffs0F f = Rank1.diffs0F (fmap runAD.f.AD)
{-# INLINE diffs0F #-}

taylor :: Fractional a => (forall s. AD s (Tower a) -> AD s (Tower a)) -> a -> a -> [a]
taylor f = Rank1.taylor (runAD.f.AD)

taylor0 :: Fractional a => (forall s. AD s (Tower a) -> AD s (Tower a)) -> a -> a -> [a]
taylor0 f = Rank1.taylor0 (runAD.f.AD)
{-# INLINE taylor0 #-}

maclaurin :: Fractional a => (forall s. AD s (Tower a) -> AD s (Tower a)) -> a -> [a]
maclaurin f = Rank1.maclaurin (runAD.f.AD)
{-# INLINE maclaurin #-}

maclaurin0 :: Fractional a => (forall s. AD s (Tower a) -> AD s (Tower a)) -> a -> [a]
maclaurin0 f = Rank1.maclaurin0 (runAD.f.AD)
{-# INLINE maclaurin0 #-}

diff :: Num a => (forall s. AD s (Tower a) -> AD s (Tower a)) -> a -> a
diff f = Rank1.diff (runAD.f.AD)
{-# INLINE diff #-}

diff' :: Num a => (forall s. AD s (Tower a) -> AD s (Tower a)) -> a -> (a, a)
diff' f = Rank1.diff' (runAD.f.AD)
{-# INLINE diff' #-}

du :: (Functor f, Num a) => (forall s. f (AD s (Tower a)) -> AD s (Tower a)) -> f (a, a) -> a
du f = Rank1.du (runAD.f. fmap AD)
{-# INLINE du #-}

du' :: (Functor f, Num a) => (forall s. f (AD s (Tower a)) -> AD s (Tower a)) -> f (a, a) -> (a, a)
du' f = Rank1.du' (runAD.f.fmap AD)
{-# INLINE du' #-}

duF :: (Functor f, Functor g, Num a) => (forall s. f (AD s (Tower a)) -> g (AD s (Tower a))) -> f (a, a) -> g a
duF f = Rank1.duF (fmap runAD.f.fmap AD)
{-# INLINE duF #-}

duF' :: (Functor f, Functor g, Num a) => (forall s. f (AD s (Tower a)) -> g (AD s (Tower a))) -> f (a, a) -> g (a, a)
duF' f = Rank1.duF' (fmap runAD.f.fmap AD)
{-# INLINE duF' #-}

dus :: (Functor f, Num a) => (forall s. f (AD s (Tower a)) -> AD s (Tower a)) -> f [a] -> [a]
dus f = Rank1.dus (runAD.f.fmap AD)
{-# INLINE dus #-}

dus0 :: (Functor f, Num a) => (forall s. f (AD s (Tower a)) -> AD s (Tower a)) -> f [a] -> [a]
dus0 f = Rank1.dus0 (runAD.f.fmap AD)
{-# INLINE dus0 #-}

dusF :: (Functor f, Functor g, Num a) => (forall s. f (AD s (Tower a)) -> g (AD s (Tower a))) -> f [a] -> g [a]
dusF f = Rank1.dusF (fmap runAD.f.fmap AD)
{-# INLINE dusF #-}

dus0F :: (Functor f, Functor g, Num a) => (forall s. f (AD s (Tower a)) -> g (AD s (Tower a))) -> f [a] -> g [a]
dus0F f = Rank1.dus0F (fmap runAD.f.fmap AD)
{-# INLINE dus0F #-}
