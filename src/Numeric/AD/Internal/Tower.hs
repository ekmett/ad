{-# LANGUAGE Rank2Types, TypeFamilies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TemplateHaskell, DeriveDataTypeable, InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- {-# OPTIONS_HADDOCK hide, prune #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Numeric.AD.Tower.Internal
-- Copyright   : (c) Edward Kmett 2010
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

import Prelude hiding (all)
import Control.Applicative hiding ((<**>))
import Data.Foldable
import Data.Data (Data)
import Data.Typeable (Typeable)
import Language.Haskell.TH
import Numeric.AD.Internal.Classes

-- | @Tower@ is an AD 'Mode' that calculates a tangent tower by forward AD, and provides fast 'diffsUU', 'diffsUF'
newtype Tower s a = Tower { getTower :: [a] } deriving (Data, Typeable)

type instance Scalar (Tower s a) = a

instance Show a => Show (Tower s a) where
    showsPrec n (Tower as) = showParen (n > 10) $ showString "Tower " . showList as

-- Local combinators

zeroPad :: Num a => [a] -> [a]
zeroPad xs = xs ++ repeat 0
{-# INLINE zeroPad #-}

zeroPadF :: (Functor f, Num a) => [f a] -> [f a]
zeroPadF fxs@(fx:_) = fxs ++ repeat (const 0 <$> fx)
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

tangents :: Tower s a -> Tower s a
tangents (Tower []) = Tower []
tangents (Tower (_:xs)) = Tower xs
{-# INLINE tangents #-}

truncated :: Tower s a -> Bool
truncated (Tower []) = True
truncated _ = False
{-# INLINE truncated #-}

bundle :: a -> Tower s a -> Tower s a
bundle a (Tower as) = Tower (a:as)
{-# INLINE bundle #-}

withD :: (a, a) -> Tower s a
withD (a, da) = Tower [a,da]
{-# INLINE withD #-}

apply :: Num a => (Tower s a -> b) -> a -> b
apply f a = f (Tower [a,1])
{-# INLINE apply #-}

getADTower :: Tower s a -> [a]
getADTower = getTower
{-# INLINE getADTower #-}

tower :: [a] -> Tower s a
tower = Tower

instance Num a => Primal (Tower s a) where
    primal (Tower (x:_)) = x
    primal _ = 0

instance (Num a) => Mode (Tower s a) where
    auto a = Tower [a]
    zero = Tower []
    Tower [] <**> y         = auto (0 ** primal y)
    _        <**> Tower []  = auto 1
    x        <**> Tower [y] = lift1 (**y) (\z -> y *^ z <**> Tower [y-1]) x
    x        <**> y         = lift2_ (**) (\z xi yi -> (yi * z / xi, z * log xi)) x y

    (<+>) = towerPlus
    -- Tower [] <+> bs = bs
    -- as <+> Tower [] = as
    -- Tower (a:as) <+> Tower (b:bs) = Tower (c:cs)
    --     where
    --         c = a + b
    --         Tower cs = Tower as <+> Tower bs

    a *^ Tower bs = Tower (map (a*) bs)
    Tower as ^* b = Tower (map (*b) as)
    Tower as ^/ b = Tower (map (/b) as)

towerPlus :: Num a => Tower s a -> Tower s a -> Tower s a
towerPlus (Tower []) bs = bs
towerPlus as (Tower []) = as
towerPlus (Tower (a : as)) (Tower (b : bs)) =
    Tower ((a + b) : getTower (Tower as <+> Tower bs))

instance Num a => Jacobian (Tower s a) where
    type D (Tower s a) = Tower s a
    unary f dadb b = bundle (f (primal b)) (tangents b * dadb)
    lift1 f df b   = bundle (f (primal b)) (tangents b * df b)
    lift1_ f df b = a where
        a = bundle (f (primal b)) (tangents b * df a b)

    binary f dadb dadc b c = bundle (f (primal b) (primal c)) (tangents b * dadb + tangents c * dadc)
    lift2 f df b c =
      bundle (f (primal b) (primal c)) tana
      where (dadb, dadc) = df b c
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

let s = VarT (mkName "s") in
  deriveNumeric id (ConT ''Tower `AppT` s) s
