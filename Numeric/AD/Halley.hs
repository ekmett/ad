{-# LANGUAGE Rank2Types, BangPatterns, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Halley
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Root finding using Halley's rational method (the second in 
-- the class of Householder methods). Assumes the function is three 
-- times continuously differentiable and converges cubically when 
-- progress can be made.
-- 
-----------------------------------------------------------------------------

module Numeric.AD.Halley
    (
    -- * Halley's Method (Tower AD)
      findZero
    , inverse
    , fixedPoint
    , extremum
    -- * Exposed Types
    , UU, UF, FU, FF
    , AD(..)
    , Mode(..)
    ) where

import Prelude hiding (all)
-- import Data.Foldable (all)
-- import Data.Traversable (Traversable)
import Numeric.AD.Types
import Numeric.AD.Classes
import Numeric.AD.Mode.Tower (diffs0)
import Numeric.AD.Mode.Forward (diff) -- , diff')
-- import Numeric.AD.Mode.Reverse (gradWith')
import Numeric.AD.Internal.Composition

-- | The 'findZero' function finds a zero of a scalar function using
-- Halley's method; its output is a stream of increasingly accurate
-- results.  (Modulo the usual caveats.) If the stream becomes constant
-- ("it converges"), no further elements are returned.
--
-- Examples:
--
--  > take 10 $ findZero (\\x->x^2-4) 1  -- converge to 2.0
--
--  > module Data.Complex
--  > take 10 $ findZero ((+1).(^2)) (1 :+ 1)  -- converge to (0 :+ 1)@
--
findZero :: (Fractional a, Eq a) => UU a -> a -> [a]
findZero f = go
    where
        go x = x : if y == 0 || x == xn then [] else go xn
            where
                (y:y':y'':_) = diffs0 f x
		xn = x - 2*y*y'/(2*y'*y'-y*y'')
{-# INLINE findZero #-}

-- | The 'inverse' function inverts a scalar function using
-- Halley's method; its output is a stream of increasingly accurate
-- results.  (Modulo the usual caveats.) If the stream becomes constant
-- ("it converges"), no further elements are returned.
--
-- Note: the @take 10 $ inverse sqrt 1 (sqrt 10)@ example that works for Newton's method
-- fails with Halley's method because the preconditions do not hold.

inverse :: (Fractional a, Eq a) => UU a -> a -> a -> [a]
inverse f x0 y = findZero (\x -> f x - lift y) x0
{-# INLINE inverse  #-}

-- | The 'fixedPoint' function find a fixedpoint of a scalar
-- function using Halley's method; its output is a stream of
-- increasingly accurate results.  (Modulo the usual caveats.)
-- If the stream becomes constant ("it converges"), no further
-- elements are returned.
-- 
-- > take 10 $ fixedPoint cos 1 -- converges to 0.7390851332151607
fixedPoint :: (Fractional a, Eq a) => UU a -> a -> [a]
fixedPoint f = findZero (\x -> f x - x)
{-# INLINE fixedPoint #-}

-- | The 'extremum' function finds an extremum of a scalar
-- function using Halley's method; produces a stream of increasingly
-- accurate results.  (Modulo the usual caveats.) If the stream becomes
-- constant ("it converges"), no further elements are returned.
--
-- > take 10 $ extremum cos 1 -- convert to 0 
extremum :: (Fractional a, Eq a) => UU a -> a -> [a]
extremum f = findZero (diff (decomposeMode . f . composeMode))
{-# INLINE extremum #-}

