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
  ( Index(..)
  , emptyIndex
  , addToIndex
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
  , deriv
  , primal
  ) where

import Prelude hiding (lookup)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative hiding ((<**>))
#endif
import Control.Comonad.Cofree
import Control.Monad (join)
import Data.Data
import Data.IntMap (IntMap, mapWithKey, unionWith, findWithDefault, toAscList, singleton, insertWith, lookup)
import qualified Data.IntMap as IntMap
import Data.Number.Erf
import Data.Traversable
import Data.Typeable ()
import Numeric.AD.Internal.Combinators
import Numeric.AD.Jacobian
import Numeric.AD.Mode

newtype Index = Index (IntMap Int)

emptyIndex :: Index
emptyIndex = Index IntMap.empty
{-# INLINE emptyIndex #-}

addToIndex :: Int -> Index -> Index
addToIndex k (Index m) = Index (insertWith (+) k 1 m)
{-# INLINE addToIndex #-}

indices :: Index -> [Int]
indices (Index as) = uncurry (flip replicate) `concatMap` toAscList as
{-# INLINE indices #-}

-- | We only store partials in sorted order, so the map contained in a partial
-- will only contain partials with equal or greater keys to that of the map in
-- which it was found. This should be key for efficiently computing sparse hessians.
-- there are only (n + k - 1) choose (k - 1) distinct nth partial derivatives of a
-- function with k inputs.
data Sparse a
  = Sparse !a (IntMap (Sparse a))
  | Zero
  deriving (Show, Data, Typeable)

{-

These functions are now unused.

dropMap :: Int -> IntMap a -> IntMap a
dropMap n = snd . IntMap.split (n - 1)
{-# INLINE dropMap #-}

times :: Num a => Sparse a -> Int -> Sparse a -> Sparse a
times Zero _ _ = Zero
times _ _ Zero = Zero
times a@(Sparse pa da) n b@(Sparse pb db) = Sparse (pa * pb) $
  unionWith (+)
    (fmap (* b) (dropMap n da))
    (fmap (a *) (dropMap n db))
{-# INLINE times #-}
-}

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
d fs (Zero) = 0 <$ fs
d fs (Sparse _ da) = snd $ mapAccumL (\ !n _ -> (n + 1, maybe 0 primal $ lookup n da)) 0 fs
{-# INLINE d #-}

d' :: (Traversable f, Num a) => f a -> Sparse a -> (a, f a)
d' fs Zero = (0, 0 <$ fs)
d' fs (Sparse a da) = (a, snd $ mapAccumL (\ !n _ -> (n + 1, maybe 0 primal $ lookup n da)) 0 fs)
{-# INLINE d' #-}

ds :: (Traversable f, Num a) => f b -> Sparse a -> Cofree f a
ds fs Zero = r where r = 0 :< (r <$ fs)
ds fs (as@(Sparse a _)) = a :< (go emptyIndex <$> fns) where
  fns = skeleton fs
  -- go :: Index -> Int -> Cofree f a
  go ix i = partial (indices ix') as :< (go ix' <$> fns) where
    ix' = addToIndex i ix
{-# INLINE ds #-}

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

(<**>) :: Floating a => Sparse a -> Sparse a -> Sparse a
Zero <**> y    = auto (0 ** primal y)
_    <**> Zero = auto 1
x    <**> y@(Sparse b bs)
  | IntMap.null bs = lift1 (**b) (\z -> b *^ z <**> Sparse (b-1) IntMap.empty) x
  | otherwise      = lift2_ (**) (\z xi yi -> (yi * z / xi, z * log xi)) x y

instance Num a => Mode (Sparse a) where
  type Scalar (Sparse a) = a
  auto a = Sparse a IntMap.empty
  zero = Zero
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
    a = Sparse (f pb) $ IntMap.map ((df a b) *) bs

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
-- A monomial is used to indicate order of differentiation.
-- For a k-ary function, it represented as a list of k non-negative Ints.
-- MI [n_0,n_1...n_{k-1}] denotes differentiation n_0 times with respect
-- to variable 0, n_1 times to variable 1, etc.
-- Trailing zeros omitted for efficiency.
--
-- Add 1 to variable k (i.e.differentiate once more wrt variable k).
incMonomial :: Int -> [Int] -> [Int]
incMonomial k [] = replicate k 0 ++ [1]
incMonomial 0 (a:as) = a+1:as
incMonomial k (a:as) = a:incMonomial (k-1) as

-- deriv f mi is the derivative of f of order mi (including higher derivatives).
deriv :: Sparse a -> [Int] -> Sparse a
deriv f mi = indx 0 mi f
  where indx _ [] f = f
        indx _ _ Zero = Zero
        indx v (0:as) f = indx (v+1) as f
        indx v (a:as) (Sparse _ df) = maybe Zero (indx v (a-1 : as)) (lookup v df)

-- The value of the derivative of (f*g) of order mi is
--       sum [a*primal (deriv f b)*primal (deriv g c) | (a,b,c) <- terms mi ]
-- It is a bit more complicated in mul' below, since we build the whole tree of
-- derivatives and want to prune the tree with Zeros as much as possible.
-- The number of terms in the sum for order MI as of differentiation has
-- sum (map (+1) as) terms, so this is *much* more efficient
-- than the naive recursive differentiation with 2^(sum as) terms.
-- The coefficients a, which collect equivalent derivatives, are suitable products
-- of binomial coefficients.
terms :: [Int]-> [(Integer,[Int],[Int])]
terms [] = [(1,[],[])]
terms (a:as) = concatMap (f ps) (zip (bins!!a) [0..a])
  where ps = terms as
        bins = iterate next [1]
        next xs@(_:ts) = 1 : zipWith (+) xs ts ++ [1]
        f ps (b,k) = map (\(w,ks,is) -> (w*b,(k:ks),(a-k:is))) ps

mul :: Num a => Sparse a -> Sparse a -> Sparse a
mul a b = mul' (maxKey a b) a b
  where maxKey (Sparse _ am) (Sparse _ bm) = max (maximum (-1:IntMap.keys am)) (maximum (-1:IntMap.keys bm))

mul' :: Num a => Int -> Sparse a -> Sparse a -> Sparse a
mul' _ Zero _ = Zero
mul' _ _ Zero = Zero
mul' kMax f g = Sparse (primal f * primal g) (derivs 0 [])
  where derivs v mi = IntMap.unions (map fn [v..kMax])
          where fn w
                 |and zs = IntMap.empty
                 |otherwise = IntMap.singleton w (Sparse (sum ds) (derivs w mi'))
                 where mi' = incMonomial w mi
                       (zs,ds) = unzip (map derVal (terms mi'))
        derVal (bin,mif,mig) = (isZero fder || isZero gder,
                                    fromIntegral bin * primal fder * primal gder)
           where fder = deriv f mif
                 gder = deriv g mig
