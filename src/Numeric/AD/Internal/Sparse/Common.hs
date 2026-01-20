{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_HADDOCK not-home #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2010-2026
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- common guts for Sparse.Double and Sparse mode
--
-- Handle with care.
-----------------------------------------------------------------------------
module Numeric.AD.Internal.Sparse.Common
  ( Monomial(..)
  , emptyMonomial
  , addToMonomial
  , indices
  , skeleton
  , terms
  ) where

import Data.IntMap (IntMap, toAscList, insertWith)
import qualified Data.IntMap as IntMap
import Data.Traversable

newtype Monomial = Monomial (IntMap Int)

emptyMonomial :: Monomial
emptyMonomial = Monomial IntMap.empty
{-# INLINE emptyMonomial #-}

addToMonomial :: Int -> Monomial -> Monomial
addToMonomial k (Monomial m) = Monomial (insertWith (+) k 1 m)
{-# INLINE addToMonomial #-}

indices :: Monomial -> [Int]
indices (Monomial as) = uncurry (flip replicate) `concatMap` toAscList as
{-# INLINE indices #-}

skeleton :: Traversable f => f a -> f Int
skeleton = snd . mapAccumL (\ !n _ -> (n + 1, n)) 0
{-# INLINE skeleton #-}

terms :: Monomial -> [(Integer,Monomial,Monomial)]
terms (Monomial m) = t (toAscList m) where
  t [] = [(1,emptyMonomial,emptyMonomial)]
  t ((k,a):ts) = concatMap (f (t ts)) (zip (bins!!a) [0..a]) where
    f ps (b,i) = map (\(w,Monomial mf,Monomial mg) -> (w*b,Monomial (IntMap.insert k i mf), Monomial (IntMap.insert k (a-i) mg))) ps
  bins = iterate next [1]
  next xs@(_:ts) = 1 : zipWith (+) xs ts ++ [1]
  next [] = error "impossible"
