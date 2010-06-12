{-# LANGUAGE BangPatterns, TemplateHaskell, TypeFamilies, TypeOperators, FlexibleContexts, UndecidableInstances, DeriveDataTypeable #-}
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
    ) where

import Prelude hiding (lookup)
import Control.Applicative
import Numeric.AD.Internal.Classes
import Numeric.AD.Internal.Stream
import Numeric.AD.Internal.Types
import Data.Data
import Data.Typeable ()
import qualified Data.IntMap as IntMap 
import Data.IntMap (IntMap, mapWithKey, unionWith, findWithDefault, toAscList, singleton, insertWith, lookup)
import Data.Traversable
import Language.Haskell.TH

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
-- there are only (n + k - 1) choose k distinct nth partial derivatives of a 
-- function with k inputs.
data Sparse a = Sparse a (IntMap (Sparse a)) deriving (Show, Data, Typeable)

-- | drop keys below a given value
dropMap :: Int -> IntMap a -> IntMap a
dropMap n = snd . IntMap.split (n - 1) 
{-# INLINE dropMap #-}

times :: Num a => Sparse a -> Int -> Sparse a -> Sparse a
times (Sparse a as) n (Sparse b bs) = Sparse (a * b) $
    unionWith (<+>) 
        (fmap (^* b) (dropMap n as))
        (fmap (a *^) (dropMap n bs))
{-# INLINE times #-}

vars :: (Traversable f, Num a) => f a -> f (AD Sparse a)
vars = snd . mapAccumL var 0 
    where
        var !n a = (n + 1, AD $ Sparse a $ singleton n $ lift 1)
{-# INLINE vars #-}

apply :: (Traversable f, Num a) => (f (AD Sparse a) -> b) -> f a -> b
apply f = f . vars 
{-# INLINE apply #-}

skeleton :: Traversable f => f a -> f Int
skeleton = snd . mapAccumL (\ !n _ -> (n + 1, n)) 0
{-# INLINE skeleton #-}

d :: (Traversable f, Num a) => f b -> AD Sparse a -> f a
d fs (AD (Sparse _ da)) = snd $ mapAccumL (\ !n _ -> (n + 1, maybe 0 primal $ lookup n da)) 0 fs
{-# INLINE d #-}

d' :: (Traversable f, Num a) => f a -> AD Sparse a -> (a, f a)
d' fs (AD (Sparse a da)) = (a , snd $ mapAccumL (\ !n _ -> (n + 1, maybe 0 primal $ lookup n da)) 0 fs)
{-# INLINE d' #-}

ds :: (Traversable f, Num a) => f b -> AD Sparse a -> Stream f a
ds fs (AD as@(Sparse a _)) = a :< (go emptyIndex <$> fns)
    where
        fns = skeleton fs
        -- go :: Index -> Int -> Stream f a
        go ix i = partial (indices ix') as :< (go ix' <$> fns)
            where ix' = addToIndex i ix
{-# INLINE ds #-}

{-
vvars :: Num a => Vector a -> Vector (AD Sparse a)
vvars = Vector.imap (\n a -> AD $ Sparse a $ singleton n $ lift 1)
{-# INLINE vvars #-}

vapply :: Num a => (Vector (AD Sparse a) -> b) -> Vector a -> b
vapply f = f . vvars 
{-# INLINE vapply #-}


vd :: Num a => Int -> AD Sparse a -> Vector a
vd n (AD (Sparse _ da)) = Vector.generate n $ \i -> maybe 0 primal $ lookup i da
{-# INLINE vd #-}

vd' :: Num a => Int -> AD Sparse a -> (a, Vector a)
vd' n (AD (Sparse a da)) = (a , Vector.generate n $ \i -> maybe 0 primal $ lookup i da)
{-# INLINE vd' #-}

vds :: Num a => Int -> AD Sparse a -> Stream Vector a
vds n (AD as@(Sparse a _)) = a :< Vector.generate n (go emptyIndex)
    where
        go ix i = partial (indices ix') as :< Vector.generate n (go ix')
            where ix' = addToIndex i ix
{-# INLINE vds #-}
-}

partial :: Num a => [Int] -> Sparse a -> a
partial []     (Sparse a _)  = a
partial (n:ns) (Sparse _ da) = partial ns $ findWithDefault (lift 0) n da
{-# INLINE partial #-}

spartial :: Num a => [Int] -> Sparse a -> Maybe a
spartial [] (Sparse a _) = Just a
spartial (n:ns) (Sparse _ da) = do
    a' <- lookup n da
    spartial ns a'
{-# INLINE spartial #-}

instance Primal Sparse where
    primal (Sparse a _) = a

instance Lifted Sparse => Mode Sparse where
    lift a = Sparse a (IntMap.empty)
    Sparse a as <+> Sparse b bs = Sparse (a + b) $ unionWith (<+>) as bs
    Sparse a as ^* b = Sparse (a * b) $ fmap (^* b) as
    a *^ Sparse b bs = Sparse (a * b) $ fmap (a *^) bs
    Sparse a as ^/ b = Sparse (a / b) $ fmap (^/ b) as

instance Lifted Sparse => Jacobian Sparse where
    type D Sparse = Sparse
    unary f dadb (Sparse pb bs) = Sparse (f pb) $ mapWithKey (times dadb) bs
    lift1 f df b@(Sparse pb bs) = Sparse (f pb) $ mapWithKey (times (df b)) bs
    lift1_ f df b@(Sparse pb bs) = a where
        a = Sparse (f pb) $ mapWithKey (times (df a b)) bs

    binary f dadb dadc (Sparse pb db) (Sparse pc dc) = Sparse (f pb pc) $ 
        unionWith (<+>) 
            (mapWithKey (times dadb) db)
            (mapWithKey (times dadc) dc)

    lift2 f df b@(Sparse pb db) c@(Sparse pc dc) = Sparse (f pb pc) da where
        (dadb, dadc) = df b c
        da = unionWith (<+>) 
            (mapWithKey (times dadb) db)
            (mapWithKey (times dadc) dc)
        
    lift2_ f df b@(Sparse pb db) c@(Sparse pc dc) = a where
        (dadb, dadc) = df a b c
        a = Sparse (f pb pc) da
        da = unionWith (<+>) 
            (mapWithKey (times dadb) db)
            (mapWithKey (times dadc) dc)

deriveLifted id $ conT ''Sparse
