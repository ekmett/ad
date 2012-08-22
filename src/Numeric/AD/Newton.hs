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
    , inverse
    , fixedPoint
    , extremum
    -- * Gradient Ascent/Descent (Reverse AD)
    , gradientDescent
    , gradientAscent
    ) where

import Prelude hiding (all)
import Data.Foldable (all)
import Data.Traversable (Traversable)
import Numeric.AD.Types
import Numeric.AD.Mode.Forward (diff, diff')
import Numeric.AD.Mode.Reverse (gradWith')
import Numeric.AD.Internal.Composition

-- | The 'findZero' function finds a zero of a scalar function using
-- Newton's method; its output is a stream of increasingly accurate
-- results.  (Modulo the usual caveats.)
--
-- Examples:
--
-- >>> take 10 $ findZero (\x->x^2-4) 1
-- [1.0,2.5,2.05,2.000609756097561,2.0000000929222947,2.000000000000002,2.0]
--
-- >>> import Data.Complex
-- >>> last $ take 10 $ findZero ((+1).(^2)) (1 :+ 1)
-- 0.0 :+ 1.0
findZero :: (Fractional a, Eq a) => (forall s. Mode s => AD s a -> AD s a) -> a -> [a]
findZero f = go
    where
        go x = x : if y == 0 then [] else go (x - y/y')
            where
                (y,y') = diff' f x
{-# INLINE findZero #-}

-- | The 'inverse' function inverts a scalar function using
-- Newton's method; its output is a stream of increasingly accurate
-- results.  (Modulo the usual caveats.)
--
-- Example:
--
-- >>> last $ take 10 $ inverse sqrt 1 (sqrt 10)
-- 10.0
inverse :: (Fractional a, Eq a) => (forall s. Mode s => AD s a -> AD s a) -> a -> a -> [a]
inverse f x0 y = findZero (\x -> f x - lift y) x0
{-# INLINE inverse  #-}

-- | The 'fixedPoint' function find a fixedpoint of a scalar
-- function using Newton's method; its output is a stream of
-- increasingly accurate results.  (Modulo the usual caveats.)
--
-- >>> last $ take 10 $ fixedPoint cos 1
-- 0.7390851332151607
fixedPoint :: (Fractional a, Eq a) => (forall s. Mode s => AD s a -> AD s a) -> a -> [a]
fixedPoint f = findZero (\x -> f x - x)
{-# INLINE fixedPoint #-}

-- | The 'extremum' function finds an extremum of a scalar
-- function using Newton's method; produces a stream of increasingly
-- accurate results.  (Modulo the usual caveats.)
--
-- >>> last $ take 10 $ extremum cos 1
-- 0.0
extremum :: (Fractional a, Eq a) => (forall s. Mode s => AD s a -> AD s a) -> a -> [a]
extremum f = findZero (diff (decomposeMode . f . composeMode))
{-# INLINE extremum #-}

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
