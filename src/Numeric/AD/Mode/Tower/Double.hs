{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   : (c) Edward Kmett 2010-2026
-- License     : BSD3
-- Maintainer  : ekmett@gmail.com
-- Stability   : experimental
-- Portability : GHC only
--
-- Higher order derivatives via a \"dual number tower\".
--
-----------------------------------------------------------------------------
module Numeric.AD.Mode.Tower.Double
  ( AD
  , TowerDouble
  , auto
  -- * Taylor Series
  , taylor
  , taylor0
  -- * Maclaurin Series
  , maclaurin
  , maclaurin0
  -- * Derivatives
  , diff    -- first derivative of (Double -> a)
  , diff'   -- answer and first derivative of (Double -> a)
  , diffs   -- answer and all derivatives of (Double -> a)
  , diffs0  -- zero padded derivatives of (Double -> a)
  , diffsF  -- answer and all derivatives of (Double -> f a)
  , diffs0F -- zero padded derivatives of (Double -> f a)
  -- * Directional Derivatives
  , du      -- directional derivative of (Double -> a)
  , du'     -- answer and directional derivative of (Double -> a)
  , dus     -- answer and all directional derivatives of (Double -> a)
  , dus0    -- answer and all zero padded directional derivatives of (Double -> a)
  , duF     -- directional derivative of (Double -> f a)
  , duF'    -- answer and directional derivative of (Double -> f a)
  , dusF    -- answer and all directional derivatives of (Double -> f a)
  , dus0F   -- answer and all zero padded directional derivatives of (Double -> a)
  ) where

import qualified Numeric.AD.Rank1.Tower.Double as Rank1
import Numeric.AD.Internal.Tower.Double (TowerDouble)
import Numeric.AD.Internal.Type (AD(..))
import Numeric.AD.Mode

diffs :: (forall s. AD s TowerDouble -> AD s TowerDouble) -> Double -> [Double]
diffs f = Rank1.diffs (runAD.f.AD)
{-# INLINE diffs #-}

diffs0 :: (forall s. AD s TowerDouble -> AD s TowerDouble) -> Double -> [Double]
diffs0 f = Rank1.diffs0 (runAD.f.AD)
{-# INLINE diffs0 #-}

diffsF :: Functor f => (forall s. AD s TowerDouble -> f (AD s TowerDouble)) -> Double -> f [Double]
diffsF f = Rank1.diffsF (fmap runAD.f.AD)
{-# INLINE diffsF #-}

diffs0F :: Functor f => (forall s. AD s TowerDouble -> f (AD s TowerDouble)) -> Double -> f [Double]
diffs0F f = Rank1.diffs0F (fmap runAD.f.AD)
{-# INLINE diffs0F #-}

taylor :: (forall s. AD s TowerDouble -> AD s TowerDouble) -> Double -> Double -> [Double]
taylor f = Rank1.taylor (runAD.f.AD)

taylor0 :: (forall s. AD s TowerDouble -> AD s TowerDouble) -> Double -> Double -> [Double]
taylor0 f = Rank1.taylor0 (runAD.f.AD)
{-# INLINE taylor0 #-}

maclaurin :: (forall s. AD s TowerDouble -> AD s TowerDouble) -> Double -> [Double]
maclaurin f = Rank1.maclaurin (runAD.f.AD)
{-# INLINE maclaurin #-}

maclaurin0 :: (forall s. AD s TowerDouble -> AD s TowerDouble) -> Double -> [Double]
maclaurin0 f = Rank1.maclaurin0 (runAD.f.AD)
{-# INLINE maclaurin0 #-}

diff :: (forall s. AD s TowerDouble -> AD s TowerDouble) -> Double -> Double
diff f = Rank1.diff (runAD.f.AD)
{-# INLINE diff #-}

diff' :: (forall s. AD s TowerDouble -> AD s TowerDouble) -> Double -> (Double, Double)
diff' f = Rank1.diff' (runAD.f.AD)
{-# INLINE diff' #-}

du :: Functor f => (forall s. f (AD s TowerDouble) -> AD s TowerDouble) -> f (Double, Double) -> Double
du f = Rank1.du (runAD.f. fmap AD)
{-# INLINE du #-}

du' :: Functor f => (forall s. f (AD s TowerDouble) -> AD s TowerDouble) -> f (Double, Double) -> (Double, Double)
du' f = Rank1.du' (runAD.f.fmap AD)
{-# INLINE du' #-}

duF :: (Functor f, Functor g) => (forall s. f (AD s TowerDouble) -> g (AD s TowerDouble)) -> f (Double, Double) -> g Double
duF f = Rank1.duF (fmap runAD.f.fmap AD)
{-# INLINE duF #-}

duF' :: (Functor f, Functor g) => (forall s. f (AD s TowerDouble) -> g (AD s TowerDouble)) -> f (Double, Double) -> g (Double, Double)
duF' f = Rank1.duF' (fmap runAD.f.fmap AD)
{-# INLINE duF' #-}

dus :: Functor f => (forall s. f (AD s TowerDouble) -> AD s TowerDouble) -> f [Double] -> [Double]
dus f = Rank1.dus (runAD.f.fmap AD)
{-# INLINE dus #-}

dus0 :: Functor f => (forall s. f (AD s TowerDouble) -> AD s TowerDouble) -> f [Double] -> [Double]
dus0 f = Rank1.dus0 (runAD.f.fmap AD)
{-# INLINE dus0 #-}

dusF :: (Functor f, Functor g) => (forall s. f (AD s TowerDouble) -> g (AD s TowerDouble)) -> f [Double] -> g [Double]
dusF f = Rank1.dusF (fmap runAD.f.fmap AD)
{-# INLINE dusF #-}

dus0F :: (Functor f, Functor g) => (forall s. f (AD s TowerDouble) -> g (AD s TowerDouble)) -> f [Double] -> g [Double]
dus0F f = Rank1.dus0F (fmap runAD.f.fmap AD)
{-# INLINE dus0F #-}
