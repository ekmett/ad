{-# LANGUAGE Rank2Types, BangPatterns, FlexibleContexts, ScopedTypeVariables #-}
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
    , conjugateGradientDescent
    , conjugateGradientAscent
    ) where

import Prelude hiding (all, mapM, sum)
import Data.Functor
import Data.Foldable (all, sum)
import Data.Reflection (Reifies)
import Data.Traversable
import Numeric.AD.Types
import Numeric.AD.Mode.Forward (diff, diff')
import Numeric.AD.Mode.Reverse (grad, gradWith')
import Numeric.AD.Internal.Classes (Lifted)
import Numeric.AD.Internal.Combinators
import Numeric.AD.Internal.Composition
import Numeric.AD.Internal.Forward (Forward)
import Numeric.AD.Internal.Reverse (Reverse, Tape)

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
-- >>> import Data.Complex
-- >>> last $ take 10 $ findZero ((+1).(^2)) (1 :+ 1)
-- 0.0 :+ 1.0
findZero :: (Fractional a, Eq a) => (forall s. AD Forward s a -> AD Forward s a) -> a -> [a]
findZero f = go where
  go x = x : if x == xn then [] else go xn where
    (y,y') = diff' f x
    xn = x - y/y'
{-# INLINE findZero #-}

-- | The 'inverse' function inverts a scalar function using
-- Newton's method; its output is a stream of increasingly accurate
-- results.  (Modulo the usual caveats.) If the stream becomes
-- constant ("it converges"), no further elements are returned.
--
-- Example:
--
-- >>> last $ take 10 $ inverse sqrt 1 (sqrt 10)
-- 10.0
inverse :: (Fractional a, Eq a) => (forall s. AD Forward s a -> AD Forward s a) -> a -> a -> [a]
inverse f x0 y = findZero (\x -> f x - auto y) x0
{-# INLINE inverse  #-}

-- | The 'fixedPoint' function find a fixedpoint of a scalar
-- function using Newton's method; its output is a stream of
-- increasingly accurate results.  (Modulo the usual caveats.)
--
-- If the stream becomes constant ("it converges"), no further
-- elements are returned.
--
-- >>> last $ take 10 $ fixedPoint cos 1
-- 0.7390851332151607
fixedPoint :: (Fractional a, Eq a) => (forall s. AD Forward s a -> AD Forward s a) -> a -> [a]
fixedPoint f = findZero (\x -> f x - x)
{-# INLINE fixedPoint #-}

-- | The 'extremum' function finds an extremum of a scalar
-- function using Newton's method; produces a stream of increasingly
-- accurate results.  (Modulo the usual caveats.) If the stream
-- becomes constant ("it converges"), no further elements are returned.
--
-- >>> last $ take 10 $ extremum cos 1
-- 0.0
extremum :: (Fractional a, Eq a) => (forall s s'. AD (ComposeMode Forward Forward s') s a -> AD (ComposeMode Forward Forward s') s a) -> a -> [a]
extremum f = findZero (diff (decomposeMode . f . composeMode))
{-# INLINE extremum #-}

-- | The 'gradientDescent' function performs a multivariate
-- optimization, based on the naive-gradient-descent in the file
-- @stalingrad\/examples\/flow-tests\/pre-saddle-1a.vlad@ from the
-- VLAD compiler Stalingrad sources.  Its output is a stream of
-- increasingly accurate results.  (Modulo the usual caveats.)
--
-- It uses reverse mode automatic differentiation to compute the gradient.
gradientDescent :: (Traversable f, Fractional a, Ord a) => (forall s. (Reifies s Tape, Lifted (Reverse s)) => f (AD (Reverse s) s a) -> AD (Reverse s) s a) -> f a -> [f a]
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

-- | Perform a gradient descent using reverse mode automatic differentiation to compute the gradient.
gradientAscent :: (Traversable f, Fractional a, Ord a) => (forall s. (Reifies s Tape, Lifted (Reverse s)) => f (AD (Reverse s) s a) -> AD (Reverse s) s a) -> f a -> [f a]
gradientAscent f = gradientDescent (negate . f)
{-# INLINE gradientAscent #-}

-- | Perform a conjugate gradient descent using reverse mode automatic differentiation to compute the gradient, and using forward-on-forward mode for computing extrema.
conjugateGradientDescent :: (Traversable f, Fractional a, Ord a) => (forall m s. Mode m => f (AD m s a) -> AD m s a) -> f a -> [f a]
conjugateGradientDescent f x0 = takeWhile (all (\a -> a == a)) (go x0 d0 d0)
  where
    dot x y = sum $ zipWithT (*) x y
    d0 = negate <$> grad f x0
    go xi ri di = xi : go xi1 ri1 di1
      where
        ai  = last $ take 20 $ extremum (\a -> f $ zipWithT (\x d -> auto x + a * auto d) xi di) 0
        xi1 = zipWithT (\x d -> x + ai*d) xi di
        ri1 = negate <$> grad f xi1
        bi1 = max 0 $ dot ri1 (zipWithT (-) ri1 ri) / dot ri1 ri1
        di1 = zipWithT (\r d -> r * bi1*d) ri1 di
{-# INLINE conjugateGradientDescent #-}

-- | Perform a conjugate gradient ascent using reverse mode automatic differentiation to compute the gradient.
conjugateGradientAscent :: (Traversable f, Fractional a, Ord a) => (forall m s. Mode m => f (AD m s a) -> AD m s a) -> f a -> [f a]
conjugateGradientAscent f = conjugateGradientDescent (negate . f)
{-# INLINE conjugateGradientAscent #-}
