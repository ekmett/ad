-- {-# OPTIONS_HADDOCK hide, prune #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Internal.Var
-- Copyright   :  (c) Edward Kmett 2012
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Variables used for reverse-mode automatic differentiation.
-----------------------------------------------------------------------------

module Numeric.AD.Internal.Var
    ( Var(..)
    , bind
    , unbind
    , unbindMap
    , unbindWith
    , unbindMapWithDefault
    , Variable(..)
    , vary
    ) where

import Prelude hiding (mapM)
import Data.Array
import Data.IntMap (IntMap, findWithDefault)
import Data.Traversable (Traversable, mapM)
import Numeric.AD.Internal.Types
import Numeric.AD.Internal.Classes

-- | Used to mark variables for inspection during the reverse pass
class Primal v => Var v where
    var   :: a -> Int -> v a
    varId :: v a -> Int

instance Var f => Var (AD f s) where
    var a v = AD (var a v)
    varId (AD v) = varId v

-- A simple fresh variable supply monad
newtype S a = S { runS :: Int -> (a,Int) }
instance Monad S where
    return a = S (\s -> (a,s))
    S g >>= f = S (\s -> let (a,s') = g s in runS (f a) s')

bind :: (Traversable f, Var v) => f a -> (f (v a), (Int,Int))
bind xs = (r,(0,hi)) where
  (r,hi) = runS (mapM freshVar xs) 0
  freshVar a = S (\s -> let s' = s + 1 in s' `seq` (var a s, s'))

unbind :: (Functor f, Var v)  => f (v a) -> Array Int a -> f a
unbind xs ys = fmap (\v -> ys ! varId v) xs

unbindWith :: (Functor f, Var v, Num a) => (a -> b -> c) -> f (v a) -> Array Int b -> f c
unbindWith f xs ys = fmap (\v -> f (primal v) (ys ! varId v)) xs

unbindMap :: (Functor f, Var v, Num a) => f (v a) -> IntMap a -> f a
unbindMap xs ys = fmap (\v -> findWithDefault 0 (varId v) ys) xs

unbindMapWithDefault :: (Functor f, Var v, Num a) => b -> (a -> b -> c) -> f (v a) -> IntMap b -> f c
unbindMapWithDefault z f xs ys = fmap (\v -> f (primal v) $ findWithDefault z (varId v) ys) xs

data Variable a = Variable a {-# UNPACK #-} !Int

instance Var Variable where
  var = Variable
  varId (Variable _ i) = i

instance Primal Variable where
  primal (Variable a _) = a

vary :: Var f => Variable a -> f a
vary (Variable a i) = var a i
