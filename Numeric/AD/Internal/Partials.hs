module Numeric.AD.Internal.Binomial where

import Data.Reflection
import Data.Vector

(<*>) :: Partials a -> Partials a -> Partials a
Partials a as <*> Partials b bs = Partials (a * b) $ 
    zipWithTails (*b) (a*) (\da db -> a * db + da * db) as bs

newtype Key = Key (IntMap Int)

data Partials a = Partials a [Partials a]

partial :: Num a => Int -> [Partials a]
partial 0 = [Partials 0 []]
partial n = 0 : part (n-1)

vars :: Traversable f => f a -> (Int, f (AD Binomial a))
vars = mapAccumL (\!n a -> (n + 1, Partials a (partial n))) 0

zipWithTail :: (a -> a -> a) -> [a] -> [a] -> [a] 
zipWithTail f (a:as) (b:bs) = f a b : zipWithTail f as bs
zipWithTail f [] as = as
zipWithTail f as _  = as

zipWithTails :: (a -> c) -> (b -> c) -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWithTails l r f (a:as) (b:bs) = f a b : zipWithTails l r f as bs
zipWithTails _ r _ []     bs     = r <$> bs
zipWithTails l _ _ as     _      = l <$> as

instance Primal Partials where
    primal (Partials a _) = a

instance Mode Partials where
    lift a = Partials a []
    Partials a as <+> Partials b bs = Partials (a + b) $ zipWithTail (<+>) as bs
    Partials a as ^* b = Partials (a * b) $ fmap (^* b) as
    a *^ Partials b bs = Partials (a * b) $ fmap (a *^) bs
    Partials a as ^/ b = Partials (a / b) $ fmap (^/ b) as

instance Jacobian Partials where
    type D Partials = Partials
    unary f b (Partials a as) = Partials (f a) $ fmap (b <*>) as
    -- lift1 f df (Partials a as) = Partials (f a) $ zipWith df

deriveLifted id (conT ''Partials)
