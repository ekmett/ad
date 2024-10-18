{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (c) Edward Kmett 2010-2021
-- License     : BSD3
-- Maintainer  : ekmett@gmail.com
-- Stability   : experimental
-- Portability : GHC only
--
-----------------------------------------------------------------------------

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

module Numeric.AD.Internal.Tower.Double
  ( TowerDouble(..)
  , List(..)
  , zeroPad
  , zeroPadF
  , transposePadF
  , d, dl
  , d', dl'
  , withD
  , tangents
  , bundle
  , apply
  , getADTower
  , tower
  ) where

import Prelude hiding (all, sum)
import Control.Monad (join)
import Data.Foldable
import Data.Data (Data)
import Data.Number.Erf
import Numeric
import Numeric.AD.Internal.Combinators
import Numeric.AD.Jacobian
import Numeric.AD.Mode
import Text.Read
import GHC.Exts as Exts (IsList(..))
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif

-- spine lazy, value strict list of doubles
data List
  = Nil
  | {-# UNPACK #-} !Double :! List
  deriving (Eq,Ord,Data)

infixr 5 :!


instance Semigroup List where
  Nil <> xs = xs
  (x :! xs) <> ys = x :! (xs <> ys)

instance Monoid List where
  mempty = Nil
  mappend = (<>)

instance IsList List where
  type Item List = Double
  toList Nil = []
  toList (a :! as) = a : Exts.toList as
  fromList [] = Nil
  fromList (a : as) = a :! Exts.fromList as

instance Show List where
  showsPrec d = showsPrec d . Exts.toList

instance Read List where
  readPrec = Exts.fromList <$> step readPrec

lmap :: (Double -> Double) -> List -> List
lmap f (a :! as) = f a :! lmap f as
lmap _ Nil = Nil


-- | @Tower@ is an AD 'Mode' that calculates a tangent tower by forward AD, and provides fast 'diffsUU', 'diffsUF'
newtype TowerDouble = Tower { getTower :: List }
  deriving Data

instance Show TowerDouble where
  showsPrec n (Tower as) = showParen (n > 10) $ showString "Tower " . showsPrec 11 as

-- Local combinators

zeroPad :: Num a => [a] -> [a]
zeroPad xs = xs ++ repeat 0
{-# INLINE zeroPad #-}

zeroPadF :: (Functor f, Num a) => [f a] -> [f a]
zeroPadF fxs@(fx:_) = fxs ++ repeat (0 <$ fx)
zeroPadF _ = error "zeroPadF :: empty list"
{-# INLINE zeroPadF #-}

lnull :: List -> Bool
lnull Nil = True
lnull _ = False

transposePadF :: (Foldable f, Functor f) => Double -> f List -> [f Double]
transposePadF pad fx
  | all lnull fx = []
  | otherwise = fmap headPad fx : transposePadF pad (drop1 <$> fx)
  where
    headPad Nil = pad
    headPad (x :! _) = x
    drop1 (_ :! xs) = xs
    drop1 xs = xs

d :: Num a => [a] -> a
d (_:da:_) = da
d _ = 0
{-# INLINE d #-}

dl :: List -> Double
dl (_ :! da :! _) = da
dl _ = 0
{-# INLINE dl #-}

d' :: Num a => [a] -> (a, a)
d' (a:da:_) = (a, da)
d' (a:_)    = (a, 0)
d' _        = (0, 0)
{-# INLINE d' #-}

dl' :: List -> (Double, Double)
dl' (a:!da:!_) = (a, da)
dl' (a:!_)     = (a, 0)
dl' _          = (0, 0)
{-# INLINE dl' #-}

tangents :: TowerDouble -> TowerDouble
tangents (Tower Nil) = Tower Nil
tangents (Tower (_ :! xs)) = Tower xs
{-# INLINE tangents #-}

truncated :: TowerDouble -> Bool
truncated (Tower Nil) = True
truncated _ = False
{-# INLINE truncated #-}

bundle :: Double -> TowerDouble -> TowerDouble
bundle a (Tower as) = Tower (a :! as)
{-# INLINE bundle #-}

withD :: (Double, Double) -> TowerDouble
withD (a, da) = Tower (a :! da :! Nil)
{-# INLINE withD #-}

apply :: (TowerDouble -> b) -> Double -> b
apply f a = f (Tower (a :! 1 :! Nil))
{-# INLINE apply #-}

getADTower :: TowerDouble -> [Double]
getADTower = Exts.toList . getTower
{-# INLINE getADTower #-}

tower :: [Double] -> TowerDouble
tower = Tower . Exts.fromList

primal :: TowerDouble -> Double
primal (Tower (x:!_)) = x
primal _ = 0

instance Mode TowerDouble where
  type Scalar TowerDouble = Double

  auto a = Tower (a :! Nil)

  isKnownZero (Tower Nil) = True
  isKnownZero (Tower (0 :! Nil)) = True
  isKnownZero _ = False

  asKnownConstant (Tower Nil) = Just 0
  asKnownConstant (Tower (a :! Nil)) = Just a
  asKnownConstant Tower {} = Nothing

  isKnownConstant (Tower Nil) = True
  isKnownConstant (Tower (_ :! Nil)) = True
  isKnownConstant Tower {} = False

  zero = Tower Nil

  a *^ Tower bs = Tower (lmap (a*) bs)

  Tower as ^* b = Tower (lmap (*b) as)

  Tower as ^/ b = Tower (lmap (/b) as)

infixr 6 <+>

(<+>) :: TowerDouble -> TowerDouble -> TowerDouble
Tower Nil <+> bs = bs
as <+> Tower Nil = as
Tower (a:!as) <+> Tower (b:!bs) = Tower (c:!cs) where
  c = a + b
  Tower cs = Tower as <+> Tower bs

instance Jacobian TowerDouble where
  type D TowerDouble = TowerDouble
  unary f dadb b = bundle (f (primal b)) (tangents b * dadb)
  lift1 f df b   = bundle (f (primal b)) (tangents b * df b)
  lift1_ f df b = a where
    a = bundle (f (primal b)) (tangents b * df a b)

  binary f dadb dadc b c = bundle (f (primal b) (primal c)) (tangents b * dadb + tangents c * dadc)
  lift2 f df b c = bundle (f (primal b) (primal c)) tana where
     (dadb, dadc) = df b c
     tanb = tangents b
     tanc = tangents c
     tana = case (truncated tanb, truncated tanc) of
       (False, False) -> tanb * dadb + tanc * dadc
       (True, False) -> tanc * dadc
       (False, True) -> tanb * dadb
       (True, True) -> zero
  lift2_ f df b c = a where
    a0 = f (primal b) (primal c)
    da = tangents b * dadb + tangents c * dadc
    a = bundle a0 da
    (dadb, dadc) = df a b c

lzipWith :: (Double -> Double -> Double) -> List -> List -> List
lzipWith f (a :! as) (b :! bs) = f a b :! lzipWith f as bs
lzipWith _ _ _ = Nil

lsumProd3 :: List -> List -> List -> Double
lsumProd3 as0 bs0 cs0 = go as0 bs0 cs0 0 where
  go (a :! as) (b :! bs) (c :! cs) !acc = go as bs cs (a*b*c + acc)
  go _ _ _ acc = acc;

ltail :: List -> List
ltail (_ :! as) = as
ltail _ = error "ltail"

-- mul xs ys = [ sum [xs!!j * ys!!(k-j)*bin k j | j <- [0..k]] | k <- [0..] ]
-- adapted for efficiency and to handle finite lists xs, ys
mul:: TowerDouble -> TowerDouble -> TowerDouble
mul (Tower Nil) _ = Tower Nil
mul (Tower (a :! as)) (Tower bs) = Tower (convs' (1 :! Nil) (a :! Nil) as bs)
  where convs' _ _ _ Nil = Nil
        convs' ps ars as bs = lsumProd3 ps ars bs :!
              case as of
                 Nil -> convs'' (next' ps) ars bs
                 a:!as -> convs' (next ps) (a:!ars) as bs
        convs'' _ _ Nil = undefined -- convs'' never called with last argument empty
        convs'' _ _ (_:! Nil) = Nil
        convs'' ps ars (_:!bs) = lsumProd3 ps ars bs :! convs'' (next' ps) ars bs
        next xs = 1 :! lzipWith (+) xs (ltail xs) <> (1 :! Nil) -- next row in Pascal's triangle
        next' xs = lzipWith (+) xs (ltail xs) <> (1 :! Nil) -- end part of next row in Pascal's triangle

#define HEAD TowerDouble
#define BODY1(x)
#define BODY2(x,y)
#define NO_Bounded
#include <instances.h>
