{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2010-2014
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
  ) where

import Prelude hiding (all)
import Numeric.AD.Internal.Forward (Forward)
import Numeric.AD.Internal.On
import Numeric.AD.Internal.Tower (Tower)
import Numeric.AD.Mode
import Numeric.AD.Mode.Tower (diffs0)
import Numeric.AD.Mode.Forward (diff) -- , diff')

-- $setup
-- >>> import Data.Complex

-- | The 'findZero' function finds a zero of a scalar function using
-- Halley's method; its output is a stream of increasingly accurate
-- results.  (Modulo the usual caveats.) If the stream becomes constant
-- ("it converges"), no further elements are returned.
--
-- Examples:
--
-- >>> take 10 $ findZero (\x->x^2-4) 1
-- [1.0,1.8571428571428572,1.9997967892704736,1.9999999999994755,2.0]
--
-- >>> last $ take 10 $ findZero ((+1).(^2)) (1 :+ 1)
-- 0.0 :+ 1.0
findZero :: (Fractional a, Eq a) => (forall s. Tower a s -> Tower a s) -> a -> [a]
findZero f = go where
  go x = x : if x == xn then [] else go xn where
    (y:y':y'':_) = diffs0 f x
    xn = x - 2*y*y'/(2*y'*y'-y*y'')
{-# INLINE findZero #-}

-- | The 'inverse' function inverts a scalar function using
-- Halley's method; its output is a stream of increasingly accurate
-- results.  (Modulo the usual caveats.) If the stream becomes constant
-- ("it converges"), no further elements are returned.
--
-- Note: the @take 10 $ inverse sqrt 1 (sqrt 10)@ example that works for Newton's method
-- fails with Halley's method because the preconditions do not hold!
inverse :: (Fractional a, Eq a) => (forall s. Tower a s -> Tower a s) -> a -> a -> [a]
inverse f x0 y = findZero (\x -> f x - auto y) x0
{-# INLINE inverse  #-}

-- | The 'fixedPoint' function find a fixedpoint of a scalar
-- function using Halley's method; its output is a stream of
-- increasingly accurate results.  (Modulo the usual caveats.)
--
-- If the stream becomes constant ("it converges"), no further
-- elements are returned.
--
-- >>> last $ take 10 $ fixedPoint cos 1
-- 0.7390851332151607
fixedPoint :: (Fractional a, Eq a) => (forall s. Tower a s -> Tower a s) -> a -> [a]
fixedPoint f = findZero (\x -> f x - x)
{-# INLINE fixedPoint #-}


-- | The 'extremum' function finds an extremum of a scalar
-- function using Halley's method; produces a stream of increasingly
-- accurate results.  (Modulo the usual caveats.) If the stream becomes
-- constant ("it converges"), no further elements are returned.
--
-- >>> take 10 $ extremum cos 1
-- [1.0,0.29616942658570555,4.59979519460002e-3,1.6220740159042513e-8,0.0]
extremum :: (Fractional a, Eq a) => (forall s s'. On (Forward (Tower a s') s) -> On (Forward (Tower a s') s)) -> a -> [a]
extremum f = findZero (diff (off . f . On))
{-# INLINE extremum #-}
