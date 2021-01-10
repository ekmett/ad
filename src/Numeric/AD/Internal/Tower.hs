{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (c) Edward Kmett 2010-2015
-- License     : BSD3
-- Maintainer  : ekmett@gmail.com
-- Stability   : experimental
-- Portability : GHC only
--
-----------------------------------------------------------------------------

module Numeric.AD.Internal.Tower
  ( Tower(..)
  , zeroPad
  , zeroPadF
  , transposePadF
  , d
  , d'
  , withD
  , tangents
  , bundle
  , apply
  , getADTower
  , tower
  ) where

import Prelude hiding (all, sum)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative hiding ((<**>))
#endif
import Control.Monad (join)
import Data.Foldable
import Data.Data (Data)
import Data.Number.Erf
import Data.Typeable (Typeable)
import Numeric.AD.Internal.Combinators
import Numeric.AD.Jacobian
import Numeric.AD.Mode

-- | @Tower@ is an AD 'Mode' that calculates a tangent tower by forward AD, and provides fast 'diffsUU', 'diffsUF'
newtype Tower a = Tower { getTower :: [a] } deriving (Data, Typeable)

instance Show a => Show (Tower a) where
  showsPrec n (Tower as) = showParen (n > 10) $ showString "Tower " . showList as

-- Local combinators

zeroPad :: Num a => [a] -> [a]
zeroPad xs = xs ++ repeat 0
{-# INLINE zeroPad #-}

zeroPadF :: (Functor f, Num a) => [f a] -> [f a]
zeroPadF fxs@(fx:_) = fxs ++ repeat (0 <$ fx)
zeroPadF _ = error "zeroPadF :: empty list"
{-# INLINE zeroPadF #-}

transposePadF :: (Foldable f, Functor f) => a -> f [a] -> [f a]
transposePadF pad fx
  | all null fx = []
  | otherwise = fmap headPad fx : transposePadF pad (drop1 <$> fx)
  where
    headPad [] = pad
    headPad (x:_) = x
    drop1 (_:xs) = xs
    drop1 xs = xs

d :: Num a => [a] -> a
d (_:da:_) = da
d _ = 0
{-# INLINE d #-}

d' :: Num a => [a] -> (a, a)
d' (a:da:_) = (a, da)
d' (a:_)    = (a, 0)
d' _        = (0, 0)
{-# INLINE d' #-}

tangents :: Tower a -> Tower a
tangents (Tower []) = Tower []
tangents (Tower (_:xs)) = Tower xs
{-# INLINE tangents #-}

truncated :: Tower a -> Bool
truncated (Tower []) = True
truncated _ = False
{-# INLINE truncated #-}

bundle :: a -> Tower a -> Tower a
bundle a (Tower as) = Tower (a:as)
{-# INLINE bundle #-}

withD :: (a, a) -> Tower a
withD (a, da) = Tower [a,da]
{-# INLINE withD #-}

apply :: Num a => (Tower a -> b) -> a -> b
apply f a = f (Tower [a,1])
{-# INLINE apply #-}

getADTower :: Tower a -> [a]
getADTower = getTower
{-# INLINE getADTower #-}

tower :: [a] -> Tower a
tower = Tower

primal :: Num a => Tower a -> a
primal (Tower (x:_)) = x
primal _ = 0

instance Num a => Mode (Tower a) where
  type Scalar (Tower a) = a
  auto a = Tower [a]
  zero = Tower []
  a *^ Tower bs = Tower (map (a*) bs)
  Tower as ^* b = Tower (map (*b) as)
  Tower as ^/ b = Tower (map (/b) as)

infixr 6 <+>

(<+>) :: Num a => Tower a -> Tower a -> Tower a
Tower [] <+> bs = bs
as <+> Tower [] = as
Tower (a:as) <+> Tower (b:bs) = Tower (c:cs) where
  c = a + b
  Tower cs = Tower as <+> Tower bs

instance Num a => Jacobian (Tower a) where
  type D (Tower a) = Tower a
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

(<**>) :: Floating a => Tower a -> Tower a -> Tower a
Tower [] <**> y         = auto (0 ** primal y)
_        <**> Tower []  = auto 1
x        <**> Tower [y] = lift1 (**y) (\z -> y *^ z <**> Tower [y-1]) x
x        <**> y         = lift2_ (**) (\z xi yi -> (yi * z / xi, z * log xi)) x y

-- mul xs ys = [ sum [xs!!j * ys!!(k-j)*bin k j | j <- [0..k]] | k <- [0..] ]
-- adapted for efficiency and to handle finite lists xs, ys
mul:: Num a => Tower a -> Tower a -> Tower a
mul (Tower []) _ = Tower []
mul (Tower (a:as)) (Tower bs) = Tower (convs' [1] [a] as bs)
  where convs' _ _ _ [] = []
        convs' ps ars as bs = sumProd3 ps ars bs :
              case as of
                 [] -> convs'' (next' ps) ars bs
                 a:as -> convs' (next ps) (a:ars) as bs
        convs'' _ _ [] = undefined -- convs'' never called with last argument empty
        convs'' _ _ [_] = []
        convs'' ps ars (_:bs) = sumProd3 ps ars bs : convs'' (next' ps) ars bs
        next xs = 1 : zipWith (+) xs (tail xs) ++ [1] -- next row in Pascal's triangle
        next' xs = zipWith (+) xs (tail xs) ++ [1] -- end part of next row in Pascal's triangle
        sumProd3 as bs cs = sum (zipWith3 (\x y z -> x*y*z) as bs cs)

#define HEAD Tower a
#include <instances.h>
