{-# LANGUAGE FlexibleContexts, TypeFamilies, UndecidableInstances #-}
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
import Numeric.AD.Internal.Classes

-- | Used to mark variables for inspection during the reverse pass
class Primal v => Var v where
    var   :: Scalar v -> Int -> v
    varId :: v -> Int

-- A simple fresh variable supply monad
newtype S a = S { runS :: Int -> (a,Int) }
instance Monad S where
    return a = S (\s -> (a,s))
    S g >>= f = S (\s -> let (a,s') = g s in runS (f a) s')

bind :: (Traversable f, Var v) => f (Scalar v) -> (f v, (Int,Int))
bind xs = (r,(0,hi)) where
  (r,hi) = runS (mapM freshVar xs) 0
  freshVar a = S (\s -> let s' = s + 1 in s' `seq` (var a s, s'))

unbind :: (Functor f, Var v)  => f v -> Array Int (Scalar v) -> f (Scalar v)
unbind xs ys = fmap (\v -> ys ! varId v) xs

unbindWith :: (Functor f, Var v, Num (Scalar v)) => (Scalar v -> b -> c) -> f v -> Array Int b -> f c
unbindWith f xs ys = fmap (\v -> f (primal v) (ys ! varId v)) xs

unbindMap :: (Functor f, Var v, Num (Scalar v)) => f v -> IntMap (Scalar v) -> f (Scalar v)
unbindMap xs ys = fmap (\v -> findWithDefault 0 (varId v) ys) xs

unbindMapWithDefault :: (Functor f, Var v, Num (Scalar v)) => b -> (Scalar v -> b -> c) -> f v -> IntMap b -> f c
unbindMapWithDefault z f xs ys = fmap (\v -> f (primal v) $ findWithDefault z (varId v) ys) xs

data Variable a = Variable a {-# UNPACK #-} !Int

type instance Scalar (Variable a) = a

instance (Num a) => Var (Variable a) where
  var = Variable
  varId (Variable _ i) = i

instance (Num a) => Primal (Variable a) where
  primal (Variable a _) = a

vary :: Var f => Variable (Scalar f) -> f
vary (Variable a i) = var a i
