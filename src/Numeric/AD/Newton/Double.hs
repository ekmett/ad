{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2015
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------

module Numeric.AD.Newton.Double
  (
  -- * Newton's Method (Forward AD)
    findZero
  , inverse
  , fixedPoint
  , extremum
  -- * Gradient Ascent/Descent (Reverse AD)
  , conjugateGradientDescent
  , conjugateGradientAscent
  ) where

import Data.Foldable (all, sum)
import Data.Traversable
import Numeric.AD.Internal.Combinators
import Numeric.AD.Internal.Forward (Forward)
import Numeric.AD.Internal.Forward.Double (ForwardDouble)
import Numeric.AD.Internal.On
import Numeric.AD.Internal.Or
import Numeric.AD.Internal.Type (AD(..))
import Numeric.AD.Mode
import Numeric.AD.Rank1.Kahn as Kahn (Kahn, grad)
import qualified Numeric.AD.Rank1.Newton.Double as Rank1
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
findZero :: (forall s. AD s ForwardDouble -> AD s ForwardDouble) -> Double -> [Double]
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
inverse :: (forall s. AD s ForwardDouble -> AD s ForwardDouble) -> Double -> Double -> [Double]
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
fixedPoint :: (forall s. AD s ForwardDouble -> AD s ForwardDouble) -> Double -> [Double]
fixedPoint f = Rank1.fixedPoint (runAD.f.AD)
{-# INLINE fixedPoint #-}

-- | The 'extremum' function finds an extremum of a scalar
-- function using Newton's method; produces a stream of increasingly
-- accurate results.  (Modulo the usual caveats.) If the stream
-- becomes constant ("it converges"), no further elements are returned.
--
-- >>> last $ take 10 $ extremum cos 1
-- 0.0
extremum :: (forall s. AD s (On (Forward ForwardDouble)) -> AD s (On (Forward ForwardDouble))) -> Double -> [Double]
extremum f = Rank1.extremum (runAD.f.AD)
{-# INLINE extremum #-}

-- | Perform a conjugate gradient descent using reverse mode automatic differentiation to compute the gradient, and using forward-on-forward mode for computing extrema.
--
-- >>> let sq x = x * x
-- >>> let rosenbrock [x,y] = sq (1 - x) + 100 * sq (y - sq x)
-- >>> rosenbrock [0,0]
-- 1
-- >>> rosenbrock (conjugateGradientDescent rosenbrock [0, 0] !! 5) < 0.1
-- True
conjugateGradientDescent
  :: Traversable f
  => (forall s. Chosen s => f (Or s (On (Forward ForwardDouble)) (Kahn Double)) -> Or s (On (Forward ForwardDouble)) (Kahn Double))
  -> f Double -> [f Double]
conjugateGradientDescent f = conjugateGradientAscent (negate . f)
{-# INLINE conjugateGradientDescent #-}

lfu :: Functor f => (f (Or F a b) -> Or F a b) -> f a -> a
lfu f = runL . f . fmap L

rfu :: Functor f => (f (Or T a b) -> Or T a b) -> f b -> b
rfu f = runR . f . fmap R

-- | Perform a conjugate gradient ascent using reverse mode automatic differentiation to compute the gradient.
conjugateGradientAscent
  :: Traversable f
  => (forall s. Chosen s => f (Or s (On (Forward ForwardDouble)) (Kahn Double)) -> Or s (On (Forward ForwardDouble)) (Kahn Double))
  -> f Double -> [f Double]
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
