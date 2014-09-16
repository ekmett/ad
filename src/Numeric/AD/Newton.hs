{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2010-2014
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
  , stochasticGradientDescent
  ) where

import Data.Foldable (all, sum)
import Data.Reflection (Reifies)
import Data.Traversable
import Numeric.AD.Internal.Combinators
import Numeric.AD.Internal.Forward (Forward)
import Numeric.AD.Internal.On
import Numeric.AD.Internal.Or
import Numeric.AD.Internal.Reverse (Reverse, Tape)
import Numeric.AD.Internal.Type (AD(..))
import Numeric.AD.Mode
import Numeric.AD.Mode.Reverse as Reverse (gradWith, gradWith')
import Numeric.AD.Rank1.Kahn as Kahn (Kahn, grad)
import qualified Numeric.AD.Rank1.Newton as Rank1
import Prelude hiding (all, mapM, sum)

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
findZero :: (Fractional a, Eq a) => (forall s. AD s (Forward a) -> AD s (Forward a)) -> a -> [a]
findZero f = Rank1.findZero (runAD.f.AD)
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
inverse :: (Fractional a, Eq a) => (forall s. AD s (Forward a) -> AD s (Forward a)) -> a -> a -> [a]
inverse f = Rank1.inverse (runAD.f.AD)
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
fixedPoint :: (Fractional a, Eq a) => (forall s. AD s (Forward a) -> AD s (Forward a)) -> a -> [a]
fixedPoint f = Rank1.fixedPoint (runAD.f.AD)
{-# INLINE fixedPoint #-}

-- | The 'extremum' function finds an extremum of a scalar
-- function using Newton's method; produces a stream of increasingly
-- accurate results.  (Modulo the usual caveats.) If the stream
-- becomes constant ("it converges"), no further elements are returned.
--
-- >>> last $ take 10 $ extremum cos 1
-- 0.0
extremum :: (Fractional a, Eq a) => (forall s. AD s (On (Forward (Forward a))) -> AD s (On (Forward (Forward a)))) -> a -> [a]
extremum f = Rank1.extremum (runAD.f.AD)
{-# INLINE extremum #-}

-- | The 'gradientDescent' function performs a multivariate
-- optimization, based on the naive-gradient-descent in the file
-- @stalingrad\/examples\/flow-tests\/pre-saddle-1a.vlad@ from the
-- VLAD compiler Stalingrad sources.  Its output is a stream of
-- increasingly accurate results.  (Modulo the usual caveats.)
--
-- It uses reverse mode automatic differentiation to compute the gradient.
gradientDescent :: (Traversable f, Fractional a, Ord a) => (forall s. Reifies s Tape => f (Reverse s a) -> Reverse s a) -> f a -> [f a]
gradientDescent f x0 = go x0 fx0 xgx0 0.1 (0 :: Int)
  where
    (fx0, xgx0) = Reverse.gradWith' (,) f x0
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
        (fx1, xgx1) = Reverse.gradWith' (,) f x1
{-# INLINE gradientDescent #-}

-- | The 'stochasticGradientDescent' function approximates
-- the true gradient of the constFunction by a gradient at
-- a single example. As the algorithm sweeps through the training 
-- set, it performs the update for each training example.
--
-- It uses reverse mode automatic differentiation to compute the gradient
-- The learning rate is constant through out, and is set to 0.001
stochasticGradientDescent :: (Traversable f, Fractional a, Ord a) 
  => (forall s. Reifies s Tape => f (Scalar a) -> f (Reverse s a) -> Reverse s a) 
  -> [f (Scalar a)]
  -> f a 
  -> [f a]
stochasticGradientDescent errorSingle d0 x0 = go xgx0 0.001 dLeft
  where
    dLeft = tail $ cycle d0
    xgx0 = Reverse.gradWith (,) (errorSingle (head d0)) x0
    go xgx !eta d
      | eta ==0       = []
      | otherwise     = x1 : go xgx1 eta (tail d)
      where
        x1 = fmap (\(xi, gxi) -> xi - eta * gxi) xgx
        (_, xgx1) = Reverse.gradWith' (,) (errorSingle (head d)) x1
{-# INLINE stochasticGradientDescent #-}

-- | Perform a gradient descent using reverse mode automatic differentiation to compute the gradient.
gradientAscent :: (Traversable f, Fractional a, Ord a) => (forall s. Reifies s Tape => f (Reverse s a) -> Reverse s a) -> f a -> [f a]
gradientAscent f = gradientDescent (negate . f)
{-# INLINE gradientAscent #-}

-- | Perform a conjugate gradient descent using reverse mode automatic differentiation to compute the gradient, and using forward-on-forward mode for computing extrema.
--
-- >>> let sq x = x * x
-- >>> let rosenbrock [x,y] = sq (1 - x) + 100 * sq (y - sq x)
-- >>> rosenbrock [0,0]
-- 1
-- >>> rosenbrock (conjugateGradientDescent rosenbrock [0, 0] !! 5) < 0.1
-- True
conjugateGradientDescent
  :: (Traversable f, Ord a, Fractional a)
  => (forall s. Chosen s => f (Or s (On (Forward (Forward a))) (Kahn a)) -> Or s (On (Forward (Forward a))) (Kahn a))
  -> f a -> [f a]
conjugateGradientDescent f = conjugateGradientAscent (negate . f)
{-# INLINE conjugateGradientDescent #-}

lfu :: Functor f => (f (Or F a b) -> Or F a b) -> f a -> a
lfu f = runL . f . fmap L

rfu :: Functor f => (f (Or T a b) -> Or T a b) -> f b -> b
rfu f = runR . f . fmap R

-- | Perform a conjugate gradient ascent using reverse mode automatic differentiation to compute the gradient.
conjugateGradientAscent
  :: (Traversable f, Ord a, Fractional a)
  => (forall s. Chosen s => f (Or s (On (Forward (Forward a))) (Kahn a)) -> Or s (On (Forward (Forward a))) (Kahn a))
  -> f a -> [f a]
conjugateGradientAscent f x0 = takeWhile (all (\a -> a == a)) (go x0 d0 d0 delta0)
  where
    dot x y = sum $ zipWithT (*) x y
    d0 = Kahn.grad (rfu f) x0
    delta0 = dot d0 d0
    go xi _ri di deltai = xi : go xi1 ri1 di1 deltai1
      where
        ai = last $ take 20 $ Rank1.extremum (\a -> lfu f $ zipWithT (\x d -> auto x + a * auto d) xi di) 0
        xi1 = zipWithT (\x d -> x + ai*d) xi di
        ri1 = Kahn.grad (rfu f) xi1
        deltai1 = dot ri1 ri1
        bi1 = deltai1 / deltai
        di1 = zipWithT (\r d -> r + bi1 * d) ri1 di
{-# INLINE conjugateGradientAscent #-}
