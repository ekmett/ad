{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (c) Edward Kmett 2010-2026
-- License     : BSD3
-- Maintainer  : ekmett@gmail.com
-- Stability   : experimental
-- Portability : GHC only
--
-- A dense forward AD based on representable functors. This allows for much larger
-- forward mode data types than 'Numeric.AD.Internal.Dense, as we only need
-- the ability to compare the representation of a functor for equality, rather
-- than put the representation on in a straight line like you have to with
-- 'Traversable'.
-----------------------------------------------------------------------------

module Numeric.AD.Internal.Dense.Representable
  ( Repr(..)
  , ds
  , ds'
  , vars
  , apply
  ) where

import Control.Monad (join)
import Data.Functor.Rep
import Data.Data ()
import Data.Number.Erf
import Numeric
import Numeric.AD.Internal.Combinators
import Numeric.AD.Internal.Identity
import Numeric.AD.Jacobian
import Numeric.AD.Mode

data Repr f a
  = Lift !a
  | Repr !a (f a)
  | Zero

instance Show a => Show (Repr f a) where
  showsPrec d (Lift a)    = showsPrec d a
  showsPrec d (Repr a _) = showsPrec d a
  showsPrec _ Zero        = showString "0"

ds :: f a -> Repr f a -> f a
ds _ (Repr _ da) = da
ds z _ = z
{-# INLINE ds #-}

ds' :: Num a => f a -> Repr f a -> (a, f a)
ds' _ (Repr a da) = (a, da)
ds' z (Lift a) = (a, z)
ds' z Zero = (0, z)
{-# INLINE ds' #-}

-- Bind variables and count inputs
vars :: (Representable f, Eq (Rep f), Num a) => f a -> f (Repr f a)
vars = imapRep $ \i a -> Repr a $ tabulate $ \j -> if i == j then 1 else 0
{-# INLINE vars #-}

apply :: (Representable f, Eq (Rep f), Num a) => (f (Repr f a) -> b) -> f a -> b
apply f as = f (vars as)
{-# INLINE apply #-}

primal :: Num a => Repr f a -> a
primal Zero = 0
primal (Lift a) = a
primal (Repr a _) = a

instance (Representable f, Num a) => Mode (Repr f a) where
  type Scalar (Repr f a) = a
  asKnownConstant (Lift a) = Just a
  asKnownConstant Zero = Just 0
  asKnownConstant _ = Nothing
  isKnownConstant Repr{} = False
  isKnownConstant _ = True
  isKnownZero Zero = True
  isKnownZero _ = False
  auto = Lift
  zero = Zero
  _ *^ Zero      = Zero
  a *^ Lift b    = Lift (a * b)
  a *^ Repr b db = Repr (a * b) $ fmap (a*) db
  Zero      ^* _ = Zero
  Lift a    ^* b = Lift (a * b)
  Repr a da ^* b = Repr (a * b) $ fmap (*b) da
  Zero      ^/ _ = Zero
  Lift a    ^/ b = Lift (a / b)
  Repr a da ^/ b = Repr (a / b) $ fmap (/b) da

(<+>) :: (Representable f, Num a) => Repr f a -> Repr f a -> Repr f a
Zero      <+> a         = a
a         <+> Zero      = a
Lift a    <+> Lift b    = Lift (a + b)
Lift a    <+> Repr b db = Repr (a + b) db
Repr a da <+> Lift b    = Repr (a + b) da
Repr a da <+> Repr b db = Repr (a + b) $ liftR2 (+) da db

instance (Representable f, Num a) => Jacobian (Repr f a) where
  type D (Repr f a) = Id a
  unary f _         Zero        = Lift (f 0)
  unary f _         (Lift b)    = Lift (f b)
  unary f (Id dadb) (Repr b db) = Repr (f b) (fmap (dadb *) db)

  lift1 f _  Zero        = Lift (f 0)
  lift1 f _  (Lift b)    = Lift (f b)
  lift1 f df (Repr b db) = Repr (f b) (fmap (dadb *) db) where
    Id dadb = df (Id b)

  lift1_ f _  Zero         = Lift (f 0)
  lift1_ f _  (Lift b)     = Lift (f b)
  lift1_ f df (Repr b db) = Repr a (fmap (dadb *) db) where
    a = f b
    Id dadb = df (Id a) (Id b)

  binary f _          _        Zero        Zero        = Lift (f 0 0)
  binary f _          _        Zero        (Lift c)    = Lift (f 0 c)
  binary f _          _        (Lift b)    Zero        = Lift (f b 0)
  binary f _          _        (Lift b)    (Lift c)    = Lift (f b c)
  binary f _         (Id dadc) Zero        (Repr c dc) = Repr (f 0 c) $ fmap (* dadc) dc
  binary f _         (Id dadc) (Lift b)    (Repr c dc) = Repr (f b c) $ fmap (* dadc) dc
  binary f (Id dadb) _         (Repr b db) Zero        = Repr (f b 0) $ fmap (dadb *) db
  binary f (Id dadb) _         (Repr b db) (Lift c)    = Repr (f b c) $ fmap (dadb *) db
  binary f (Id dadb) (Id dadc) (Repr b db) (Repr c dc) = Repr (f b c) $ liftR2 productRule db dc where
    productRule dbi dci = dadb * dbi + dci * dadc

  lift2 f _  Zero        Zero        = Lift (f 0 0)
  lift2 f _  Zero        (Lift c)    = Lift (f 0 c)
  lift2 f _  (Lift b)    Zero        = Lift (f b 0)
  lift2 f _  (Lift b)    (Lift c)    = Lift (f b c)
  lift2 f df Zero        (Repr c dc) = Repr (f 0 c) $ fmap (*dadc) dc where dadc = runId (snd (df (Id 0) (Id c)))
  lift2 f df (Lift b)    (Repr c dc) = Repr (f b c) $ fmap (*dadc) dc where dadc = runId (snd (df (Id b) (Id c)))
  lift2 f df (Repr b db) Zero        = Repr (f b 0) $ fmap (dadb*) db where dadb = runId (fst (df (Id b) (Id 0)))
  lift2 f df (Repr b db) (Lift c)    = Repr (f b c) $ fmap (dadb*) db where dadb = runId (fst (df (Id b) (Id c)))
  lift2 f df (Repr b db) (Repr c dc) = Repr (f b c) da where
    (Id dadb, Id dadc) = df (Id b) (Id c)
    da = liftR2 productRule db dc
    productRule dbi dci = dadb * dbi + dci * dadc

  lift2_ f _  Zero     Zero           = Lift (f 0 0)
  lift2_ f _  Zero     (Lift c)       = Lift (f 0 c)
  lift2_ f _  (Lift b) Zero           = Lift (f b 0)
  lift2_ f _  (Lift b) (Lift c)       = Lift (f b c)
  lift2_ f df Zero     (Repr c dc)    = Repr a $ fmap (*dadc) dc where
    a = f 0 c
    (_, Id dadc) = df (Id a) (Id 0) (Id c)
  lift2_ f df (Lift b) (Repr c dc)    = Repr a $ fmap (*dadc) dc where
    a = f b c
    (_, Id dadc) = df (Id a) (Id b) (Id c)
  lift2_ f df (Repr b db) Zero        = Repr a $ fmap (dadb*) db where
    a = f b 0
    (Id dadb, _) = df (Id a) (Id b) (Id 0)
  lift2_ f df (Repr b db) (Lift c)    = Repr a $ fmap (dadb*) db where
    a = f b c
    (Id dadb, _) = df (Id a) (Id b) (Id c)
  lift2_ f df (Repr b db) (Repr c dc) = Repr a $ liftR2 productRule db dc where
    a = f b c
    (Id dadb, Id dadc) = df (Id a) (Id b) (Id c)
    productRule dbi dci = dadb * dbi + dci * dadc

mul :: (Representable f, Num a) => Repr f a -> Repr f a -> Repr f a
mul = lift2 (*) (\x y -> (y, x))

#define BODY1(x)   (Representable f, x) =>
#define BODY2(x,y) (Representable f, x, y) =>
#define HEAD (Repr f a)
#include "instances.h"
