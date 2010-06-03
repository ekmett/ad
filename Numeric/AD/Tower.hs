{-# LANGUAGE Rank2Types, TypeFamilies, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances, TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Numeric.AD.Tower
-- Copyright   : (c) Edward Kmett 2010
-- License     : BSD3
-- Maintainer  : ekmett@gmail.com
-- Stability   : experimental
-- Portability : GHC only 
--
-- Mixed-Mode Automatic Differentiation.
-- 
-- For reverse mode AD we use 'System.Mem.StableName.StableName' to recover sharing information from 
-- the tape to avoid combinatorial explosion, and thus run asymptotically faster
-- than it could without such sharing information, but the use of side-effects
-- contained herein is benign.
--
-----------------------------------------------------------------------------

module Numeric.AD.Tower
    ( 
    -- * multiple derivatives
      diffsUU, diffs0UU
    , diffsUF, diffs0UF 
    -- * common access patterns
    , diffs, diffs0
    -- * taylor series
    , taylor, taylor0
    -- * internals
    , Lifted(..)
    , Mode(..)
    , AD(..)
    , Tower(..)
    , tangents
    , bundle
    ) where

import Control.Applicative
import Numeric.AD.Classes
import Numeric.AD.Internal
import Language.Haskell.TH
import Data.List (mapAccumL)

-- | @Tower@ is an AD 'Mode' that calculates a tangent tower by forward AD, and provides fast 'diffsUU', 'diffsUF'
newtype Tower a = Tower { getTower :: [a] } deriving (Show)

-- | extract the tangent tower from a tangent bundle
tangents :: Tower a -> Tower a
tangents (Tower []) = Tower []
tangents (Tower (_:xs)) = Tower xs
{-# INLINE tangents #-}

-- | bundle a point with tangent vector tower at that point
bundle :: a -> Tower a -> Tower a
bundle a (Tower as) = Tower (a:as)
{-# INLINE bundle #-}

apply :: Num a => (AD Tower a -> b) -> a -> b
apply f a = f (AD (Tower [a,1]))
{-# INLINE apply #-}

getADTower :: AD Tower a -> [a]
getADTower (AD t) = getTower t

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
        a = bundle (f (primal b) (primal c)) (tangents b *! dadb +! tangents c *! dadc)
        (dadb, dadc) = df a b c

deriveLifted (conT ''Tower)

diffsUU :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> [a]
diffsUU f a = getADTower $ apply f a 
{-# INLINE diffsUU #-}


diffs0UU :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> [a]
diffs0UU f a = zeroPad (diffsUU f a)
{-# INLINE diffs0UU #-}

diffs0UF :: (Functor f, Num a) => (forall s. Mode s => AD s a -> f (AD s a)) -> a -> f [a]
diffs0UF f a = (zeroPad . getADTower) <$> apply f a
{-# INLINE diffs0UF #-}

diffsUF :: (Functor f, Num a) => (forall s. Mode s => AD s a -> f (AD s a)) -> a -> f [a]
diffsUF f a = getADTower <$> apply f a
{-# INLINE diffsUF #-}

zeroPad :: Num a => [a] -> [a]
zeroPad xs = xs ++ repeat 0 
{-# INLINE zeroPad #-}

-- zeroPadF :: (Num a, Functor f) => [f a] -> [f a]
-- zeroPadF fxs@(fx:_) = fxs ++ repeat (fmap (const 0) fx)
-- {-# INLINE zeroPadF #-}

diffs :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> [a]
diffs = diffsUU
{-# INLINE diffs #-}

diffs0 :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> [a]
diffs0 = diffs0UU
{-# INLINE diffs0 #-}

taylor :: Fractional a => (forall s. Mode s => AD s a -> AD s a) -> a -> a -> [a]
taylor f x dx = snd $ 
    mapAccumL (\a x -> diag (a + x)) 0 $
    zipWith3 (\x y z -> x * y * z)    
             (diffsUU f x)
             recipFactorials
             (powers x)
    where
        powers x = iterate (*x) 1
        recipFactorials = snd $ mapAccumL (\a i -> (a / fromIntegral i, a)) 1 [1..]
        diag x = (x, x)

taylor0 :: Fractional a => (forall s. Mode s => AD s a -> AD s a) -> a -> a -> [a]
taylor0 f x dx = zeroPad (taylor f x dx)
{-# INLINE taylor0 #-}
