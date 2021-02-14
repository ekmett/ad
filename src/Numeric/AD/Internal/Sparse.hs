{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_HADDOCK not-home #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2010-2015
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Unsafe and often partial combinators intended for internal usage.
--
-- Handle with care.
-----------------------------------------------------------------------------
module Numeric.AD.Internal.Sparse
  ( Monomial(..)
  , emptyMonomial
  , addToMonomial
  , indices
  , Sparse(..)
  , apply
  , vars
  , d, d', ds
  , skeleton
  , spartial
  , partial
  , vgrad
  , vgrad'
  , vgrads
  , Grad(..)
  , Grads(..)
  , terms
  , primal
  ) where

import Prelude hiding (lookup)
import Control.Comonad.Cofree
import Control.Monad (join, guard)
import Data.Data
import Data.IntMap (IntMap, unionWith, findWithDefault, toAscList, singleton, insertWith, lookup)
import qualified Data.IntMap as IntMap
import Data.Number.Erf
import Data.Traversable
import Data.Typeable ()
import Numeric.AD.Internal.Combinators
import Numeric.AD.Jacobian
import Numeric.AD.Mode

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

-- | We only store partials in sorted order, so the map contained in a partial
-- will only contain partials with equal or greater keys to that of the map in
-- which it was found. This should be key for efficiently computing sparse hessians.
-- there are only @n + k - 1@ choose @k@ distinct nth partial derivatives of a
-- function with k inputs.
data Sparse a
  = Sparse !a (IntMap (Sparse a))
  | Zero
  deriving (Show, Data, Typeable)

vars :: (Traversable f, Num a) => f a -> f (Sparse a)
vars = snd . mapAccumL var 0 where
  var !n a = (n + 1, Sparse a $ singleton n $ auto 1)
{-# INLINE vars #-}

apply :: (Traversable f, Num a) => (f (Sparse a) -> b) -> f a -> b
apply f = f . vars
{-# INLINE apply #-}

skeleton :: Traversable f => f a -> f Int
skeleton = snd . mapAccumL (\ !n _ -> (n + 1, n)) 0
{-# INLINE skeleton #-}

d :: (Traversable f, Num a) => f b -> Sparse a -> f a
d fs Zero = 0 <$ fs
d fs (Sparse _ da) = snd $ mapAccumL (\ !n _ -> (n + 1, maybe 0 primal $ lookup n da)) 0 fs
{-# INLINE d #-}

d' :: (Traversable f, Num a) => f a -> Sparse a -> (a, f a)
d' fs Zero = (0, 0 <$ fs)
d' fs (Sparse a da) = (a, snd $ mapAccumL (\ !n _ -> (n + 1, maybe 0 primal $ lookup n da)) 0 fs)
{-# INLINE d' #-}

ds :: (Traversable f, Num a) => f b -> Sparse a -> Cofree f a
ds fs Zero = r where r = 0 :< (r <$ fs)
ds fs as@(Sparse a _) = a :< (go emptyMonomial <$> fns) where
  fns = skeleton fs
  -- go :: Monomial -> Int -> Cofree f a
  go ix i = partial (indices ix') as :< (go ix' <$> fns) where
    ix' = addToMonomial i ix
{-# INLINE ds #-}

partialS :: Num a => [Int] -> Sparse a -> Sparse a
partialS []     a             = a
partialS (n:ns) (Sparse _ da) = partialS ns $ findWithDefault Zero n da
partialS _      Zero          = Zero
{-# INLINE partialS #-}

partial :: Num a => [Int] -> Sparse a -> a
partial []     (Sparse a _)  = a
partial (n:ns) (Sparse _ da) = partial ns $ findWithDefault (auto 0) n da
partial _      Zero          = 0
{-# INLINE partial #-}

spartial :: Num a => [Int] -> Sparse a -> Maybe a
spartial [] (Sparse a _) = Just a
spartial (n:ns) (Sparse _ da) = do
  a' <- lookup n da
  spartial ns a'
spartial _  Zero         = Nothing
{-# INLINE spartial #-}

primal :: Num a => Sparse a -> a
primal (Sparse a _) = a
primal Zero = 0

instance Num a => Mode (Sparse a) where
  type Scalar (Sparse a) = a
  auto a = Sparse a IntMap.empty
  zero = Zero
  isKnownZero Zero = True
  isKnownZero _ = False
  isKnownConstant Zero = True
  isKnownConstant (Sparse _ m) = null m
  asKnownConstant Zero = Just 0
  asKnownConstant (Sparse a m) = a <$ guard (null m)
  Zero        ^* _ = Zero
  Sparse a as ^* b = Sparse (a * b) $ fmap (^* b) as
  _ *^ Zero        = Zero
  a *^ Sparse b bs = Sparse (a * b) $ fmap (a *^) bs
  Zero        ^/ _ = Zero
  Sparse a as ^/ b = Sparse (a / b) $ fmap (^/ b) as

infixr 6 <+>

(<+>) :: Num a => Sparse a -> Sparse a -> Sparse a
Zero <+> a = a
a <+> Zero = a
Sparse a as <+> Sparse b bs = Sparse (a + b) $ unionWith (<+>) as bs

-- The instances for Jacobian for Sparse and Tower are almost identical;
-- could easily be made exactly equal by small changes.
instance Num a => Jacobian (Sparse a) where
  type D (Sparse a) = Sparse a
  unary f _ Zero = auto (f 0)
  unary f dadb (Sparse pb bs) = Sparse (f pb) $ IntMap.map (* dadb) bs

  lift1 f _ Zero = auto (f 0)
  lift1 f df b@(Sparse pb bs) = Sparse (f pb) $ IntMap.map (* df b) bs

  lift1_ f _  Zero = auto (f 0)
  lift1_ f df b@(Sparse pb bs) = a where
    a = Sparse (f pb) $ IntMap.map (df a b *) bs

  binary f _    _    Zero           Zero           = auto (f 0 0)
  binary f _    dadc Zero           (Sparse pc dc) = Sparse (f 0  pc) $ IntMap.map (dadc *) dc
  binary f dadb _    (Sparse pb db) Zero           = Sparse (f pb 0 ) $ IntMap.map (dadb *) db
  binary f dadb dadc (Sparse pb db) (Sparse pc dc) = Sparse (f pb pc) $
    unionWith (<+>)  (IntMap.map (dadb *) db) (IntMap.map (dadc *) dc)

  lift2 f _  Zero             Zero = auto (f 0 0)
  lift2 f df Zero c@(Sparse pc dc) = Sparse (f 0 pc) $ IntMap.map (dadc *) dc where dadc = snd (df zero c)
  lift2 f df b@(Sparse pb db) Zero = Sparse (f pb 0) $ IntMap.map (* dadb) db where dadb = fst (df b zero)
  lift2 f df b@(Sparse pb db) c@(Sparse pc dc) = Sparse (f pb pc) da where
    (dadb, dadc) = df b c
    da = unionWith (<+>) (IntMap.map (dadb *) db) (IntMap.map (dadc *) dc)

  lift2_ f _  Zero             Zero = auto (f 0 0)
  lift2_ f df b@(Sparse pb db) Zero = a where a = Sparse (f pb 0) (IntMap.map (fst (df a b zero) *) db)
  lift2_ f df Zero c@(Sparse pc dc) = a where a = Sparse (f 0 pc) (IntMap.map (* snd (df a zero c)) dc)
  lift2_ f df b@(Sparse pb db) c@(Sparse pc dc) = a where
    (dadb, dadc) = df a b c
    a = Sparse (f pb pc) da
    da = unionWith (<+>) (IntMap.map (dadb *) db) (IntMap.map (dadc *) dc)


#define HEAD Sparse a
#include "instances.h"

class Num a => Grad i o o' a | i -> a o o', o -> a i o', o' -> a i o where
  pack :: i -> [Sparse a] -> Sparse a
  unpack :: ([a] -> [a]) -> o
  unpack' :: ([a] -> (a, [a])) -> o'

instance Num a => Grad (Sparse a) [a] (a, [a]) a where
  pack i _ = i
  unpack f = f []
  unpack' f = f []

instance Grad i o o' a => Grad (Sparse a -> i) (a -> o) (a -> o') a where
  pack f (a:as) = pack (f a) as
  pack _ [] = error "Grad.pack: logic error"
  unpack f a = unpack (f . (a:))
  unpack' f a = unpack' (f . (a:))

vgrad :: Grad i o o' a => i -> o
vgrad i = unpack (unsafeGrad (pack i)) where
  unsafeGrad f as = d as $ apply f as
{-# INLINE vgrad #-}

vgrad' :: Grad i o o' a => i -> o'
vgrad' i = unpack' (unsafeGrad' (pack i)) where
  unsafeGrad' f as = d' as $ apply f as
{-# INLINE vgrad' #-}

class Num a => Grads i o a | i -> a o, o -> a i where
  packs :: i -> [Sparse a] -> Sparse a
  unpacks :: ([a] -> Cofree [] a) -> o

instance Num a => Grads (Sparse a) (Cofree [] a) a where
  packs i _ = i
  unpacks f = f []

instance Grads i o a => Grads (Sparse a -> i) (a -> o) a where
  packs f (a:as) = packs (f a) as
  packs _ [] = error "Grad.pack: logic error"
  unpacks f a = unpacks (f . (a:))

vgrads :: Grads i o a => i -> o
vgrads i = unpacks (unsafeGrads (packs i)) where
  unsafeGrads f as = ds as $ apply f as
{-# INLINE vgrads #-}

isZero :: Sparse a -> Bool
isZero Zero = True
isZero _ = False

-- |
-- The value of the derivative of (f*g) of order mi is
--
-- @
-- 'sum' [a * 'primal' ('partialS' ('indices' b) f) * 'primal' ('partialS' ('indices' c) g) | (a,b,c) <- 'terms' mi ]
-- @
--
-- It is a bit more complicated in 'mul' below, since we build the whole tree of
-- derivatives and want to prune the tree with 'Zero's as much as possible.
-- The number of terms in the sum for order mi as of differentiation has
-- @'sum' ('map' (+1) as)@ terms, so this is *much* more efficient
-- than the naive recursive differentiation with @2^'sum' as@ terms.
-- The coefficients @a@, which collect equivalent derivatives, are suitable products
-- of binomial coefficients.
terms :: Monomial -> [(Integer,Monomial,Monomial)]
terms (Monomial m) = t (toAscList m) where
  t [] = [(1,emptyMonomial,emptyMonomial)]
  t ((k,a):ts) = concatMap (f (t ts)) (zip (bins!!a) [0..a]) where
    f ps (b,i) = map (\(w,Monomial mf,Monomial mg) -> (w*b,Monomial (IntMap.insert k i mf), Monomial (IntMap.insert k (a-i) mg))) ps
  bins = iterate next [1]
  next xs@(_:ts) = 1 : zipWith (+) xs ts ++ [1]
  next [] = error "impossible"

mul :: Num a => Sparse a -> Sparse a -> Sparse a
mul Zero _ = Zero
mul _ Zero = Zero
mul f@(Sparse _ am) g@(Sparse _ bm) = Sparse (primal f * primal g) (derivs 0 emptyMonomial) where
  derivs v mi = IntMap.unions (map fn [v..kMax]) where
    fn w
      | and zs = IntMap.empty
      | otherwise = IntMap.singleton w (Sparse (sum ds) (derivs w mi'))
      where
        mi' = addToMonomial w mi
        (zs,ds) = unzip (map derVal (terms mi'))
        derVal (bin,mif,mig) = (isZero fder || isZero gder, fromIntegral bin * primal fder * primal gder) where
          fder = partialS (indices mif) f
          gder = partialS (indices mig) g
  kMax = maybe (-1) (fst.fst) (IntMap.maxViewWithKey am) `max` maybe (-1) (fst.fst) (IntMap.maxViewWithKey bm)
