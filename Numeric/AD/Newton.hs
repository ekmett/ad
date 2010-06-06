{-# LANGUAGE Rank2Types, BangPatterns, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Newton
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------

module Numeric.AD.Newton
    (
    -- * Newton's Method (Forward AD)
      findZero
    , findZeroM
    , inverse
    , inverseM
    , fixedPoint
    , fixedPointM
    , extremum
    , extremumM
    -- * Gradient Ascent/Descent (Reverse AD)
    , gradientDescent
    , gradientDescentM
    , gradientAscent
    , gradientAscentM
    -- * Exposed Types
    , AD(..)
    , Mode(..)
    ) where

import Prelude hiding (all)
import Control.Monad (liftM)
import Data.MList
import Numeric.AD.Classes
import Numeric.AD.Internal
import Data.Foldable (all)
import Data.Traversable (Traversable)
import Numeric.AD.Forward (diff, diff', diffM, diffM')
import Numeric.AD.Reverse (gradWith', gradWithM')
import Numeric.AD.Internal.Composition

-- | The 'findZero' function finds a zero of a scalar function using
-- Newton's method; its output is a stream of increasingly accurate
-- results.  (Modulo the usual caveats.)
--
-- Examples:
--
--  > take 10 $ findZero (\\x->x^2-4) 1  -- converge to 2.0
--
--  > module Data.Complex
--  > take 10 $ findZero ((+1).(^2)) (1 :+ 1)  -- converge to (0 :+ 1)@
--
findZero :: Fractional a => (forall s. Mode s => AD s a -> AD s a) -> a -> [a]
findZero f = go
    where
        go x = x : go (x - y/y') 
            where
                (y,y') = diff' f x
{-# INLINE findZero #-}

findZeroM :: (Monad m, Fractional a) => (forall s. Mode s => AD s a -> m (AD s a)) -> a -> MList m a
findZeroM f x0 = MList (go x0)
    where
        go x = return $ 
               MCons x $ 
               MList $ do
                (y,y') <- diffM' f x
                go (x - y/y')
{-# INLINE findZeroM #-}

-- | The 'inverseNewton' function inverts a scalar function using
-- Newton's method; its output is a stream of increasingly accurate
-- results.  (Modulo the usual caveats.)
--
-- Example:
--
-- > take 10 $ inverseNewton sqrt 1 (sqrt 10)  -- converges to 10
--
inverse :: Fractional a => (forall s. Mode s => AD s a -> AD s a) -> a -> a -> [a]
inverse f x0 y = findZero (\x -> f x - lift y) x0
{-# INLINE inverse  #-}

inverseM :: (Monad m, Fractional a) => (forall s. Mode s => AD s a -> m (AD s a)) -> a -> a -> MList m a
inverseM f x0 y = findZeroM (\x -> subtract (lift y) `liftM` f x) x0
{-# INLINE inverseM  #-}

-- | The 'fixedPoint' function find a fixedpoint of a scalar
-- function using Newton's method; its output is a stream of
-- increasingly accurate results.  (Modulo the usual caveats.)
-- 
-- > take 10 $ fixedPoint cos 1 -- converges to 0.7390851332151607
fixedPoint :: Fractional a => (forall s. Mode s => AD s a -> AD s a) -> a -> [a]
fixedPoint f = findZero (\x -> f x - x)
{-# INLINE fixedPoint #-}

fixedPointM :: (Monad m, Fractional a) => (forall s. Mode s => AD s a -> m (AD s a)) -> a -> MList m a
fixedPointM f = findZeroM (\x -> subtract x `liftM` f x)
{-# INLINE fixedPointM #-}

-- | The 'extremum' function finds an extremum of a scalar
-- function using Newton's method; produces a stream of increasingly
-- accurate results.  (Modulo the usual caveats.)
--
-- > take 10 $ extremum cos 1 -- convert to 0 
extremum :: Fractional a => (forall s. Mode s => AD s a -> AD s a) -> a -> [a]
extremum f = findZero (diff (decomposeMode . f . composeMode))
{-# INLINE extremum #-}

extremumM :: (Monad m, Fractional a) => (forall s. Mode s => AD s a -> m (AD s a)) -> a -> MList m a
extremumM f = findZeroM (diffM (liftM decomposeMode . f . composeMode))
{-# INLINE extremumM #-}

-- | The 'gradientDescent' function performs a multivariate
-- optimization, based on the naive-gradient-descent in the file
-- @stalingrad\/examples\/flow-tests\/pre-saddle-1a.vlad@ from the
-- VLAD compiler Stalingrad sources.  Its output is a stream of
-- increasingly accurate results.  (Modulo the usual caveats.)
--
-- It uses reverse mode automatic differentiation to compute the gradient.
gradientDescent :: (Traversable f, Fractional a, Ord a) => (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> [f a]
gradientDescent f x0 = go x0 fx0 xgx0 0.1 (0 :: Int)
    where
        (fx0, xgx0) = gradWith' (,) f x0
        go x fx xgx !eta !i
            | eta == 0     = [] -- step size is 0
            | fx1 > fx     = go x fx xgx (eta/2) 0 -- we stepped too far
            | zeroGrad xgx = [] -- gradient is 0
            | otherwise    = x1 : if i == 10
                                  then go x1 fx1 xgx1 (eta*2) 0
                                  else go x1 fx1 xgx1 eta (i+1)
            where
                zeroGrad = all (\(_,g) -> g == 0)
                x1 = fmap (\(xi,gxi) -> xi - eta * gxi) xgx
                (fx1, xgx1) = gradWith' (,) f x1
{-# INLINE gradientDescent #-}

gradientAscent :: (Traversable f, Fractional a, Ord a) => (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> [f a]
gradientAscent f = gradientDescent (negate . f)
{-# INLINE gradientAscent #-}

-- monadic gradient descent
gradientDescentM :: (Traversable f, Monad m, Fractional a, Ord a) => (forall s. Mode s => f (AD s a) -> m (AD s a)) -> f a -> MList m (f a)
gradientDescentM f x0 = MList $ do
        (fx0, xgx0) <- gradWithM' (,) f x0
        go x0 fx0 xgx0 0.1 (0 :: Int)
    where
        go x fx xgx !eta !i
            | eta == 0  = return MNil -- step size is 0
            | otherwise = do
                (fx1, xgx1) <- gradWithM' (,) f x1
                case () of
                 _ | fx1 > fx     -> go x fx xgx (eta/2) 0 -- we stepped too far
                   | zeroGrad xgx -> return MNil -- gradient is 0
                   | otherwise    -> return $ 
                                     MCons x1 $ 
                                     MList $
                                     if i == 10
                                     then go x1 fx1 xgx1 (eta*2) 0
                                     else go x1 fx1 xgx1 eta (i+1)
            where
                x1 = fmap (\(xi,gxi) -> xi - eta * gxi) xgx
                zeroGrad = all (\(_,g) -> g == 0)
{-# INLINE gradientDescentM #-}

gradientAscentM :: (Traversable f, Monad m, Fractional a, Ord a) => (forall s. Mode s => f (AD s a) -> m (AD s a)) -> f a -> MList m (f a)
gradientAscentM f = gradientDescentM (liftM negate . f)
{-# INLINE gradientAscentM #-}
