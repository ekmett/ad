{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2010-2021
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------

module Numeric.AD.Rank1.Newton
  (
  -- * Newton's Method (Forward)
    findZero
  , findZeroNoEq
  , inverse
  , inverseNoEq
  , fixedPoint
  , fixedPointNoEq
  , extremum
  , extremumNoEq
  -- * Gradient Ascent/Descent (Kahn)
  , gradientDescent
  , gradientAscent
  ) where

import Prelude hiding (all, mapM)
import Data.Foldable (all)
import Numeric.AD.Mode
import Numeric.AD.Rank1.Forward (Forward, diff, diff')
import Numeric.AD.Rank1.Kahn as Kahn (Kahn, gradWith')
import Numeric.AD.Internal.On
import Numeric.AD.Internal.Combinators (takeWhileDifferent)

-- $setup
-- >>> import Data.Complex

-- | The 'findZero' function finds a zero of a scalar function using
-- Newton's method; its output is a stream of increasingly accurate
-- results.  (Modulo the usual caveats.) If the stream becomes constant
-- ("it converges"), no further elements are returned.
--
-- Examples:
--
-- >>> take 10 $ findZero (\x->x^2-4) 1
-- [1.0,2.5,2.05,2.000609756097561,2.0000000929222947,2.000000000000002,2.0]
--
-- >>> last $ take 10 $ findZero ((+1).(^2)) (1 :+ 1)
-- 0.0 :+ 1.0
findZero :: (Fractional a, Eq a) => (Forward a -> Forward a) -> a -> [a]
findZero f = takeWhileDifferent . findZeroNoEq f
{-# INLINE findZero #-}

-- | The 'findZeroNoEq' function behaves the same as 'findZero' except that it
-- doesn't truncate the list once the results become constant. This means it
-- can be used with types without an 'Eq' instance.
findZeroNoEq :: Fractional a => (Forward a -> Forward a) -> a -> [a]
findZeroNoEq f = iterate go where
  go x = xn where
    (y,y') = diff' f x
    xn = x - y/y'
{-# INLINE findZeroNoEq #-}

-- | The 'inverse' function inverts a scalar function using
-- Newton's method; its output is a stream of increasingly accurate
-- results.  (Modulo the usual caveats.) If the stream becomes
-- constant ("it converges"), no further elements are returned.
--
-- Example:
--
-- >>> last $ take 10 $ inverse sqrt 1 (sqrt 10)
-- 10.0
inverse :: (Fractional a, Eq a) => (Forward a -> Forward a) -> a -> a -> [a]
inverse f x0 = takeWhileDifferent . inverseNoEq f x0
{-# INLINE inverse  #-}

-- | The 'inverseNoEq' function behaves the same as 'inverse' except that it
-- doesn't truncate the list once the results become constant. This means it
-- can be used with types without an 'Eq' instance.
inverseNoEq :: Fractional a => (Forward a -> Forward a) -> a -> a -> [a]
inverseNoEq f x0 y = findZeroNoEq (\x -> f x - auto y) x0
{-# INLINE inverseNoEq #-}

-- | The 'fixedPoint' function find a fixedpoint of a scalar
-- function using Newton's method; its output is a stream of
-- increasingly accurate results.  (Modulo the usual caveats.)
--
-- If the stream becomes constant ("it converges"), no further
-- elements are returned.
--
-- >>> last $ take 10 $ fixedPoint cos 1
-- 0.7390851332151607
fixedPoint :: (Fractional a, Eq a) => (Forward a -> Forward a) -> a -> [a]
fixedPoint f = takeWhileDifferent . fixedPointNoEq f
{-# INLINE fixedPoint #-}

-- | The 'fixedPointNoEq' function behaves the same as 'fixedPoint' except that
-- it doesn't truncate the list once the results become constant. This means it
-- can be used with types without an 'Eq' instance.
fixedPointNoEq :: Fractional a => (Forward a -> Forward a) -> a -> [a]
fixedPointNoEq f = findZeroNoEq (\x -> f x - x)
{-# INLINE fixedPointNoEq #-}

-- | The 'extremum' function finds an extremum of a scalar
-- function using Newton's method; produces a stream of increasingly
-- accurate results.  (Modulo the usual caveats.) If the stream
-- becomes constant ("it converges"), no further elements are returned.
--
-- >>> last $ take 10 $ extremum cos 1
-- 0.0
extremum :: (Fractional a, Eq a) => (On (Forward (Forward a)) -> On (Forward (Forward a))) -> a -> [a]
extremum f = takeWhileDifferent . extremumNoEq f
{-# INLINE extremum #-}

-- | The 'extremumNoEq' function behaves the same as 'extremum' except that it
-- doesn't truncate the list once the results become constant. This means it
-- can be used with types without an 'Eq' instance.
extremumNoEq :: Fractional a => (On (Forward (Forward a)) -> On (Forward (Forward a))) -> a -> [a]
extremumNoEq f = findZeroNoEq (diff (off . f . On))
{-# INLINE extremumNoEq #-}

-- | The 'gradientDescent' function performs a multivariate
-- optimization, based on the naive-gradient-descent in the file
-- @stalingrad\/examples\/flow-tests\/pre-saddle-1a.vlad@ from the
-- VLAD compiler Stalingrad sources.  Its output is a stream of
-- increasingly accurate results.  (Modulo the usual caveats.)
--
-- It uses reverse mode automatic differentiation to compute the gradient.
gradientDescent :: (Traversable f, Fractional a, Ord a) => (f (Kahn a) -> Kahn a) -> f a -> [f a]
gradientDescent f x0 = go x0 fx0 xgx0 0.1 (0 :: Int)
  where
    (fx0, xgx0) = Kahn.gradWith' (,) f x0
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
        (fx1, xgx1) = Kahn.gradWith' (,) f x1
{-# INLINE gradientDescent #-}

-- | Perform a gradient descent using reverse mode automatic differentiation to compute the gradient.
gradientAscent :: (Traversable f, Fractional a, Ord a) => (f (Kahn a) -> Kahn a) -> f a -> [f a]
gradientAscent f = gradientDescent (negate . f)
{-# INLINE gradientAscent #-}
