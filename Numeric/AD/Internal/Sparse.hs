module Numeric.AD.Internal.Sparse 
    ( Index(..)
    , emptyIndex
    , addToIndex
    , indices
    , Sparse(..)
    , vars
    , d
    , d'
    , ds
    , skeleton


    ) where

import qualified Data.IntMap as IntMap 
import Data.IntMap (IntMap)
import Data.Reflection
import Data.Vector

newtype Index = Index (IntMap Int)

emptyIndex :: Index
emptyIndex = Index empty

addToIndex :: Int -> Index -> Index
addToIndex k (Key m) = Index (insertWith (+) k 1 m)

indices :: Index -> [Int]
indices (Index as) = uncurry (flip replicate) `concatMap` toAscList as

-- | We only store partials in sorted order, so the map contained in a partial
-- will only contain partials with equal or greater keys to that of the map in
-- which it was found. This should be key for efficiently computing sparse hessians.
-- there are only (n + k - 1) choose k distinct nth partial derivatives of a 
-- function with k inputs.
data Sparse a = Sparse a (IntMap (Sparse a))

-- | drop keys below a given value
dropMap :: Int -> IntMap a -> IntMap a
dropMap n = snd . IntMap.split (n - 1) 

times :: Sparse a -> Int -> Sparse a -> Sparse a
times (Sparse a as) n (Sparse b bs) = Sparse (a * b) $
    IntMap.unionWith (+) 
        (mapWithKey (times b) (dropMap n as))
        (mapWithKey (times a) (dropMap n bs))

vars :: Traversable f => f a -> f (AD Sparse a)
vars = mapAccumL (\ !n a -> (n + 1, Sparse a (singleton n 1))) 0

skeleton :: f a -> f Int
skeleton = snd $ mapAccumL (\ !n _ -> (n + 1, n)) 0 fs

d :: (Traversable f, Num a) => f b -> AD Sparse a -> f a
d fs (Sparse a da) = snd $ mapAccumL (\ !n a -> (n + 1, findWithDefault 0 n da))

d' :: (Traversable f, Num a) => f a -> AD Sparse a -> (a, f a)
d' fs (Sparse a da) = (a , snd $ mapAccumL (\ !n a -> (n + 1, findWithDefault 0 n da))

ds :: (Traversable f, Num a) => f b -> AD Sparse a -> Stream f a
ds fs as = go emptyIndex as <$> skeleton fs
    where
        go :: Sparse a -> Index -> Int -> (f :> a) 
        go m ix i = partial (toList ix') m :< (go m ix' <$> fns)
            where ix' = addToIndex i ix

-- sparse :: (Traversable f, Num a) => f b -> AD Sparse a -> Stream (ComposeFunctor f Maybe) a 
-- sparse fs as = ...
    
partial :: Num a => [Int] -> Sparse a -> a
partial []     (Sparse a)    = a
partial (n:ns) (Sparse _ da) = partial ns $ findWithDefault (lift 0) n da

spartial :: Num a => [Int] -> Sparse a -> Maybe a
spartial [] (Partial a) = a
spartial (n:ns) (Sparse _ da) = do
    a' <- lookup n da
    spartial ns a'

instance Primal Sparse where
    primal (Sparse a _) = a

instance Mode Sparse where
    lift a = Sparse a (IntMap.empty)
    Sparse a as <+> Sparse b bs = Sparse (a + b) $ unionWith (<+>) as bs
    Sparse a as ^* b = Sparse (a * b) $ fmap (^* b) as
    a *^ Sparse b bs = Sparse (a * b) $ fmap (a *^) bs
    Sparse a as ^/ b = Sparse (a / b) $ fmap (^/ b) as

instance Jacobian Sparse where
    type D Sparse = Sparse
    unary f b (Sparse a as) = Sparse (f a) $ mapWithKey (guardedTimes b) as
    lift1 f df (Sparse a as) = Sparse (f a) $ mapWithKey (guardedTimes (df a)) as
    lift1_ f df (Sparse b bs) = a where
        a = Sparse (f a) (mapWithKey (guardedTimes (df a b) bs

    binary f dadb dadc (Sparse b db) (Sparse c dc) = Sparse (f b c) $ 
        IntMap.unionWith (+) 
            (mapWithKey (times dadb) db)
            (mapWithKey (times dadc) dc)

    lift2 f df (Sparse b db) (Sparse c dc) = Sparse (f b c) da where
        (dadb, dadc) = df b c
        da = IntMap.unionWith (+) 
            (mapWithKey (times dadb) db)
            (mapWithKey (times dadc) dc)
        
    lift2_ f df (Sparse b db) (Sparse c dc) = a where
        (dadb, dadc) = df a b c
        a = Sparse (f b c) da
        da = IntMap.unionWith (+) 
            (mapWithKey (times dadb) db)
            (mapWithKey (times dadc) dc)

deriveLifted id (conT ''Sparse)
