{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2010-2021
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

module Numeric.AD.Rank1.Halley.Double
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
import Numeric.AD.Internal.Tower.Double (TowerDouble)
import Numeric.AD.Mode
import Numeric.AD.Rank1.Tower.Double (diffs0)
import Numeric.AD.Rank1.Forward (diff)
import Numeric.AD.Internal.Combinators (takeWhileDifferent)

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
findZero :: (TowerDouble -> TowerDouble) -> Double -> [Double]
findZero f = takeWhileDifferent . findZeroNoEq f
{-# INLINE findZero #-}

-- | The 'findZeroNoEq' function behaves the same as 'findZero' except that it
-- doesn't truncate the list once the results become constant. This means it
-- can be used with types without an 'Eq' instance.
findZeroNoEq :: (TowerDouble -> TowerDouble) -> Double -> [Double]
findZeroNoEq f = iterate go where
  go x = xn where
    (y,y',y'') = case diffs0 f x of
                   (z:z':z'':_) -> (z,z',z'')
                   _ -> error "findZeroNoEq: Impossible (diffs0 should produce an infinite list)"
    xn = x - 2*y*y'/(2*y'*y'-y*y'') -- 9.606671960457536 bits error
       -- = x - recip (y'/y - y''/ y') -- "improved error" = 6.640625e-2 bits
       -- = x - y' / (y'/y/y' - y''/2) -- "improved error" = 1.4
#ifdef HERBIE
{-# ANN findZeroNoEq "NoHerbie" #-}
#endif
{-# INLINE findZeroNoEq #-}

-- | The 'inverse' function inverts a scalar function using
-- Halley's method; its output is a stream of increasingly accurate
-- results.  (Modulo the usual caveats.) If the stream becomes constant
-- ("it converges"), no further elements are returned.
--
-- Note: the @take 10 $ inverse sqrt 1 (sqrt 10)@ example that works for Newton's method
-- fails with Halley's method because the preconditions do not hold!
inverse :: (TowerDouble -> TowerDouble) -> Double -> Double -> [Double]
inverse f x0 = takeWhileDifferent . inverseNoEq f x0
{-# INLINE inverse  #-}

-- | The 'inverseNoEq' function behaves the same as 'inverse' except that it
-- doesn't truncate the list once the results become constant. This means it
-- can be used with types without an 'Eq' instance.
inverseNoEq :: (TowerDouble -> TowerDouble) -> Double -> Double -> [Double]
inverseNoEq f x0 y = findZeroNoEq (\x -> f x - auto y) x0
{-# INLINE inverseNoEq #-}

-- | The 'fixedPoint' function find a fixedpoint of a scalar
-- function using Halley's method; its output is a stream of
-- increasingly accurate results.  (Modulo the usual caveats.)
--
-- If the stream becomes constant ("it converges"), no further
-- elements are returned.
--
-- >>> last $ take 10 $ fixedPoint cos 1
-- 0.7390851332151607
fixedPoint :: (TowerDouble -> TowerDouble) -> Double -> [Double]
fixedPoint f = takeWhileDifferent . fixedPointNoEq f
{-# INLINE fixedPoint #-}

-- | The 'fixedPointNoEq' function behaves the same as 'fixedPoint' except that
-- it doesn't truncate the list once the results become constant. This means it
-- can be used with types without an 'Eq' instance.
fixedPointNoEq :: (TowerDouble -> TowerDouble) -> Double -> [Double]
fixedPointNoEq f = findZeroNoEq (\x -> f x - x)
{-# INLINE fixedPointNoEq #-}

-- | The 'extremum' function finds an extremum of a scalar
-- function using Halley's method; produces a stream of increasingly
-- accurate results.  (Modulo the usual caveats.) If the stream becomes
-- constant ("it converges"), no further elements are returned.
--
-- >>> take 10 $ extremum cos 1
-- [1.0,0.29616942658570555,4.59979519460002e-3,1.6220740159042513e-8,0.0]
extremum :: (On (Forward TowerDouble) -> On (Forward TowerDouble)) -> Double -> [Double]
extremum f = takeWhileDifferent . extremumNoEq f
{-# INLINE extremum #-}

-- | The 'extremumNoEq' function behaves the same as 'extremum' except that it
-- doesn't truncate the list once the results become constant. This means it
-- can be used with types without an 'Eq' instance.
extremumNoEq :: (On (Forward TowerDouble) -> On (Forward TowerDouble)) -> Double -> [Double]
extremumNoEq f = findZeroNoEq (diff (off . f . On))
{-# INLINE extremumNoEq #-}
