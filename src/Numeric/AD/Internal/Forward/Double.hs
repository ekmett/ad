{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
---- |
---- Copyright   :  (c) Edward Kmett 2010-2026
---- License     :  BSD3
---- Maintainer  :  ekmett@gmail.com
---- Stability   :  experimental
---- Portability :  GHC only
----
---- Unsafe and often partial combinators intended for internal usage.
----
---- Handle with care.
-------------------------------------------------------------------------------

module Numeric.AD.Internal.Forward.Double
  ( ForwardDouble(..)
  , bundle
  , unbundle
  , apply
  , bind
  , bind'
  , bindWith
  , bindWith'
  , transposeWith
  ) where

import Data.Foldable (toList)
import Data.Traversable (mapAccumL)
import Control.Monad (join)
import Data.Number.Erf
import Numeric
import Numeric.AD.Internal.Combinators
import Numeric.AD.Internal.Identity
import Numeric.AD.Jacobian
import Numeric.AD.Mode

data ForwardDouble = ForwardDouble { primal, tangent :: {-# UNPACK #-} !Double }
  deriving (Read, Show)

unbundle :: ForwardDouble -> (Double, Double)
unbundle (ForwardDouble a da) = (a, da)
{-# INLINE unbundle #-}

bundle :: Double -> Double -> ForwardDouble
bundle = ForwardDouble
{-# INLINE bundle #-}

apply :: (ForwardDouble -> b) -> Double -> b
apply f a = f (bundle a 1)
{-# INLINE apply #-}

instance Mode ForwardDouble where
  type Scalar ForwardDouble = Double

  auto = flip ForwardDouble 0

  zero = ForwardDouble 0 0

  isKnownZero (ForwardDouble 0 0) = True
  isKnownZero _ = False

  asKnownConstant (ForwardDouble x 0) = Just x
  asKnownConstant _ = Nothing
 
  isKnownConstant (ForwardDouble _ 0) = True
  isKnownConstant _ = False

  a *^ ForwardDouble b db = ForwardDouble (a * b) (a * db)
  ForwardDouble a da ^* b = ForwardDouble (a * b) (da * b)
  ForwardDouble a da ^/ b = ForwardDouble (a / b) (da / b)

(<+>) :: ForwardDouble -> ForwardDouble -> ForwardDouble
ForwardDouble a da <+> ForwardDouble b db = ForwardDouble (a + b) (da + db)

instance Jacobian ForwardDouble where
  type D ForwardDouble = Id Double

  unary f (Id dadb) (ForwardDouble b db) = ForwardDouble (f b) (dadb * db)

  lift1 f df (ForwardDouble b db) = ForwardDouble (f b) (dadb * db) where
    Id dadb = df (Id b)

  lift1_ f df (ForwardDouble b db) = ForwardDouble a da where
    a = f b
    Id da = df (Id a) (Id b) ^* db

  binary f (Id dadb) (Id dadc) (ForwardDouble b db) (ForwardDouble c dc) = ForwardDouble (f b c) $ dadb * db + dc * dadc

  lift2 f df (ForwardDouble b db) (ForwardDouble c dc) = ForwardDouble a da where
    a = f b c
    (Id dadb, Id dadc) = df (Id b) (Id c)
    da = dadb * db + dc * dadc

  lift2_ f df (ForwardDouble b db) (ForwardDouble c dc) = ForwardDouble a da where
    a = f b c
    (Id dadb, Id dadc) = df (Id a) (Id b) (Id c)
    da = dadb * db + dc * dadc

#define HEAD ForwardDouble
#define BODY1(x)
#define BODY2(x,y)
#define NO_Bounded
#include "instances.h"

bind :: Traversable f => (f ForwardDouble -> b) -> f Double -> f b
bind f as = snd $ mapAccumL outer (0 :: Int) as where
  outer !i _ = (i + 1, f $ snd $ mapAccumL (inner i) 0 as)
  inner !i !j a = (j + 1, if i == j then bundle a 1 else auto a)

bind' :: Traversable f => (f ForwardDouble -> b) -> f Double -> (b, f b)
bind' f as = dropIx $ mapAccumL outer (0 :: Int, b0) as where
  outer (!i, _) _ = let b = f $ snd $ mapAccumL (inner i) (0 :: Int) as in ((i + 1, b), b)
  inner !i !j a = (j + 1, if i == j then bundle a 1 else auto a)
  b0 = f (auto <$> as)
  dropIx ((_,b),bs) = (b,bs)

bindWith :: Traversable f => (Double -> b -> c) -> (f ForwardDouble -> b) -> f Double -> f c
bindWith g f as = snd $ mapAccumL outer (0 :: Int) as where
  outer !i a = (i + 1, g a $ f $ snd $ mapAccumL (inner i) 0 as)
  inner !i !j a = (j + 1, if i == j then bundle a 1 else auto a)

bindWith' :: Traversable f => (Double -> b -> c) -> (f ForwardDouble -> b) -> f Double -> (b, f c)
bindWith' g f as = dropIx $ mapAccumL outer (0 :: Int, b0) as where
  outer (!i, _) a = let b = f $ snd $ mapAccumL (inner i) (0 :: Int) as in ((i + 1, b), g a b)
  inner !i !j a = (j + 1, if i == j then bundle a 1 else auto a)
  b0 = f (auto <$> as)
  dropIx ((_,b),bs) = (b,bs)

transposeWith :: (Functor f, Foldable f, Traversable g) => (b -> f a -> c) -> f (g a) -> g b -> g c
transposeWith f as = snd . mapAccumL go xss0 where
  go xss b = (tail <$> xss, f b (head <$> xss))
  xss0 = toList <$> as

mul :: ForwardDouble -> ForwardDouble -> ForwardDouble
mul = lift2 (*) (\x y -> (y, x))
