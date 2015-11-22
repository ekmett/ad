{-# LANGUAGE CPP #-}
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

module Numeric.AD.Rank1.Newton.Double
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
  ) where

import Prelude hiding (all, mapM)
import Numeric.AD.Mode
import Numeric.AD.Rank1.Forward (Forward)
import qualified Numeric.AD.Rank1.Forward as Forward
import Numeric.AD.Rank1.Forward.Double (ForwardDouble, diff')
import Numeric.AD.Internal.On
import Numeric.AD.Internal.Combinators (takeWhileDifferent)

-- | The 'findZero' function finds a zero of a scalar function using
-- Newton's method; its output is a stream of increasingly accurate
-- results.  (Modulo the usual caveats.) If the stream becomes constant
-- ("it converges"), no further elements are returned.
--
-- Examples:
--
-- >>> take 10 $ findZero (\x->x^2-4) 1
-- [1.0,2.5,2.05,2.000609756097561,2.0000000929222947,2.000000000000002,2.0]
findZero :: (ForwardDouble -> ForwardDouble) -> Double -> [Double]
findZero f = takeWhileDifferent . findZeroNoEq f
{-# INLINE findZero #-}

-- | The 'findZeroNoEq' function behaves the same as 'findZero' except that it
-- doesn't truncate the list once the results become constant.
findZeroNoEq :: (ForwardDouble -> ForwardDouble) -> Double -> [Double]
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
inverse :: (ForwardDouble -> ForwardDouble) -> Double -> Double -> [Double]
inverse f x0 = takeWhileDifferent . inverseNoEq f x0
{-# INLINE inverse  #-}

-- | The 'inverseNoEq' function behaves the same as 'inverse' except that it
-- doesn't truncate the list once the results become constant.
inverseNoEq :: (ForwardDouble -> ForwardDouble) -> Double -> Double -> [Double]
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
fixedPoint :: (ForwardDouble -> ForwardDouble) -> Double -> [Double]
fixedPoint f = takeWhileDifferent . fixedPointNoEq f
{-# INLINE fixedPoint #-}

-- | The 'fixedPointNoEq' function behaves the same as 'fixedPoint' except that
-- doesn't truncate the list once the results become constant.
fixedPointNoEq :: (ForwardDouble -> ForwardDouble) -> Double -> [Double]
fixedPointNoEq f = findZeroNoEq (\x -> f x - x)
{-# INLINE fixedPointNoEq #-}

-- | The 'extremum' function finds an extremum of a scalar
-- function using Newton's method; produces a stream of increasingly
-- accurate results.  (Modulo the usual caveats.) If the stream
-- becomes constant ("it converges"), no further elements are returned.
--
-- >>> last $ take 10 $ extremum cos 1
-- 0.0
extremum :: (On (Forward ForwardDouble) -> On (Forward ForwardDouble)) -> Double -> [Double]
extremum f = takeWhileDifferent . extremumNoEq f
{-# INLINE extremum #-}

-- | The 'extremumNoEq' function behaves the same as 'extremum' except that it
-- doesn't truncate the list once the results become constant.
extremumNoEq :: (On (Forward ForwardDouble) -> On (Forward ForwardDouble)) -> Double -> [Double]
extremumNoEq f = findZeroNoEq (Forward.diff (off . f . On))
{-# INLINE extremumNoEq #-}
