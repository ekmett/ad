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
  , inverse
  , fixedPoint
  , extremum
  ) where

import Prelude hiding (all, mapM)
import Numeric.AD.Mode
import Numeric.AD.Rank1.Forward (Forward)
import qualified Numeric.AD.Rank1.Forward as Forward
import Numeric.AD.Rank1.Forward.Double (ForwardDouble, diff')
import Numeric.AD.Internal.On

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
findZero :: (ForwardDouble -> ForwardDouble) -> Double -> [Double]
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
inverse :: (ForwardDouble -> ForwardDouble) -> Double -> Double -> [Double]
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
fixedPoint :: (ForwardDouble -> ForwardDouble) -> Double -> [Double]
fixedPoint f = findZero (\x -> f x - x)
{-# INLINE fixedPoint #-}

-- | The 'extremum' function finds an extremum of a scalar
-- function using Newton's method; produces a stream of increasingly
-- accurate results.  (Modulo the usual caveats.) If the stream
-- becomes constant ("it converges"), no further elements are returned.
--
-- >>> last $ take 10 $ extremum cos 1
-- 0.0
extremum :: (On (Forward ForwardDouble) -> On (Forward ForwardDouble)) -> Double -> [Double]
extremum f = findZero (Forward.diff (off . f . On))
{-# INLINE extremum #-}
