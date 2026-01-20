{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2010-2026
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

module Numeric.AD.Halley.Double
  (
  -- * Halley's Method (Tower AD)
    findZero
  , inverse
  , fixedPoint
  , extremum
  ) where

import Prelude
import Numeric.AD.Internal.Forward (Forward)
import Numeric.AD.Internal.On
import Numeric.AD.Internal.Tower.Double (TowerDouble)
import Numeric.AD.Internal.Type (AD(..))
import qualified Numeric.AD.Rank1.Halley.Double as Rank1

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
findZero :: (forall s. AD s TowerDouble -> AD s TowerDouble) -> Double -> [Double]
findZero f = Rank1.findZero (runAD.f.AD)
{-# INLINE findZero #-}

-- | The 'inverse' function inverts a scalar function using
-- Halley's method; its output is a stream of increasingly accurate
-- results.  (Modulo the usual caveats.) If the stream becomes constant
-- ("it converges"), no further elements are returned.
--
-- Note: the @take 10 $ inverse sqrt 1 (sqrt 10)@ example that works for Newton's method
-- fails with Halley's method because the preconditions do not hold!
inverse :: (forall s. AD s TowerDouble -> AD s TowerDouble) -> Double -> Double -> [Double]
inverse f = Rank1.inverse (runAD.f.AD)
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
fixedPoint :: (forall s. AD s TowerDouble -> AD s TowerDouble) -> Double -> [Double]
fixedPoint f = Rank1.fixedPoint (runAD.f.AD)
{-# INLINE fixedPoint #-}

-- | The 'extremum' function finds an extremum of a scalar
-- function using Halley's method; produces a stream of increasingly
-- accurate results.  (Modulo the usual caveats.) If the stream becomes
-- constant ("it converges"), no further elements are returned.
--
-- >>> take 10 $ extremum cos 1
-- [1.0,0.29616942658570555,4.59979519460002e-3,1.6220740159042513e-8,0.0]
extremum :: (forall s. AD s (On (Forward TowerDouble)) -> AD s (On (Forward TowerDouble))) -> Double -> [Double]
extremum f = Rank1.extremum (runAD.f.AD)
{-# INLINE extremum #-}
