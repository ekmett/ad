{-# LANGUAGE Rank2Types, TypeFamilies, FlexibleContexts, UndecidableInstances, TemplateHaskell #-}
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
    , d
    , d2
    , tangents
    , bundle
    , apply
    , getADTower
    ) where

import Numeric.AD.Classes
import Numeric.AD.Internal
import Language.Haskell.TH

-- | @Tower@ is an AD 'Mode' that calculates a tangent tower by forward AD, and provides fast 'diffsUU', 'diffsUF'
newtype Tower a = Tower { getTower :: [a] } deriving (Show)

-- Local combinators

zeroPad :: Num a => [a] -> [a]
zeroPad xs = xs ++ repeat 0 
{-# INLINE zeroPad #-}

d :: Num a => [a] -> a
d (_:da:_) = da
d _ = 0
{-# INLINE d #-}

d2 :: Num a => [a] -> (a, a) 
d2 (a:da:_) = (a, da)
d2 (a:_)    = (a, 0)
d2 _        = (0, 0)
{-# INLINE d2 #-}

tangents :: Tower a -> Tower a
tangents (Tower []) = Tower []
tangents (Tower (_:xs)) = Tower xs
{-# INLINE tangents #-}

bundle :: a -> Tower a -> Tower a
bundle a (Tower as) = Tower (a:as)
{-# INLINE bundle #-}

apply :: Num a => (AD Tower a -> b) -> a -> b
apply f a = f (AD (Tower [a,1]))
{-# INLINE apply #-}

getADTower :: AD Tower a -> [a]
getADTower (AD t) = getTower t
{-# INLINE getADTower #-}

instance Primal Tower where
    primal (Tower (x:_)) = x
    primal _ = 0

instance Lifted Tower => Mode Tower where
    lift a = Tower [a]
    zero = Tower []

    Tower [] <+> bs = bs
    as <+> Tower [] = as
    Tower (a:as) <+> Tower (b:bs) = Tower (c:cs)
        where 
            c = a + b
            Tower cs = Tower as <+> Tower bs

    a *^ Tower bs = Tower (map (a*) bs)
    Tower as ^* b = Tower (map (*b) as)

    Tower as ^/ b = Tower (map (/b) as)

instance Lifted Tower => Jacobian Tower where
    type D Tower = Tower
    unary f dadb b = bundle (f (primal b)) (tangents b *! dadb)
    lift1 f df b   = bundle (f (primal b)) (tangents b *! df b)
    lift1_ f df b = a where 
        a = bundle (f (primal b)) (tangents b *! df a b)

    binary f dadb dadc b c = bundle (f (primal b) (primal c)) (tangents b *! dadb +! tangents c *! dadc) 
    lift2 f df b c = bundle (f (primal b) (primal c)) (tangents b *! dadb +! tangents c *! dadc) where
        (dadb, dadc) = df b c 
    lift2_ f df b c = a where 
        a0 = f (primal b) (primal c)
        da = tangents b *! dadb +! tangents c *! dadc
        a = bundle a0 da 
        (dadb, dadc) = df a b c

deriveLifted (conT ''Tower)
