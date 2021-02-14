{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

module Numeric.AD.Halley
  (
  -- * Halley's Method (Tower AD)
    findZero
  , findZeroNoEq
  , inverse
  , inverseNoEq
  , fixedPoint
  , fixedPointNoEq
  , extremum
  , extremumNoEq
  ) where

import Prelude
import Numeric.AD.Internal.Forward (Forward)
import Numeric.AD.Internal.On
import Numeric.AD.Internal.Tower (Tower)
import Numeric.AD.Internal.Type (AD(..))
import qualified Numeric.AD.Rank1.Halley as Rank1

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
findZero :: (Fractional a, Eq a) => (forall s. AD s (Tower a) -> AD s (Tower a)) -> a -> [a]
findZero f = Rank1.findZero (runAD.f.AD)
{-# INLINE findZero #-}

-- | The 'findZeroNoEq' function behaves the same as 'findZero' except that it
-- doesn't truncate the list once the results become constant. This means it
-- can be used with types without an 'Eq' instance.
findZeroNoEq :: Fractional a => (forall s. AD s (Tower a) -> AD s (Tower a)) -> a -> [a]
findZeroNoEq f = Rank1.findZeroNoEq (runAD.f.AD)
{-# INLINE findZeroNoEq #-}

-- | The 'inverse' function inverts a scalar function using
-- Halley's method; its output is a stream of increasingly accurate
-- results.  (Modulo the usual caveats.) If the stream becomes constant
-- ("it converges"), no further elements are returned.
--
-- Note: the @take 10 $ inverse sqrt 1 (sqrt 10)@ example that works for Newton's method
-- fails with Halley's method because the preconditions do not hold!
inverse :: (Fractional a, Eq a) => (forall s. AD s (Tower a) -> AD s (Tower a)) -> a -> a -> [a]
inverse f = Rank1.inverse (runAD.f.AD)
{-# INLINE inverse  #-}

-- | The 'inverseNoEq' function behaves the same as 'inverse' except that it
-- doesn't truncate the list once the results become constant. This means it
-- can be used with types without an 'Eq' instance.
inverseNoEq :: Fractional a => (forall s. AD s (Tower a) -> AD s (Tower a)) -> a -> a -> [a]
inverseNoEq f = Rank1.inverseNoEq (runAD.f.AD)
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
fixedPoint :: (Fractional a, Eq a) => (forall s. AD s (Tower a) -> AD s (Tower a)) -> a -> [a]
fixedPoint f = Rank1.fixedPoint (runAD.f.AD)
{-# INLINE fixedPoint #-}

-- | The 'fixedPointNoEq' function behaves the same as 'fixedPoint' except that
-- it doesn't truncate the list once the results become constant. This means it
-- can be used with types without an 'Eq' instance.
fixedPointNoEq :: Fractional a => (forall s. AD s (Tower a) -> AD s (Tower a)) -> a -> [a]
fixedPointNoEq f = Rank1.fixedPointNoEq (runAD.f.AD)
{-# INLINE fixedPointNoEq #-}

-- | The 'extremum' function finds an extremum of a scalar
-- function using Halley's method; produces a stream of increasingly
-- accurate results.  (Modulo the usual caveats.) If the stream becomes
-- constant ("it converges"), no further elements are returned.
--
-- >>> take 10 $ extremum cos 1
-- [1.0,0.29616942658570555,4.59979519460002e-3,1.6220740159042513e-8,0.0]
extremum :: (Fractional a, Eq a) => (forall s. AD s (On (Forward (Tower a))) -> AD s (On (Forward (Tower a)))) -> a -> [a]
extremum f = Rank1.extremum (runAD.f.AD)
{-# INLINE extremum #-}

-- | The 'extremumNoEq' function behaves the same as 'extremum' except that it
-- doesn't truncate the list once the results become constant. This means it
-- can be used with types without an 'Eq' instance.
extremumNoEq :: Fractional a => (forall s. AD s (On (Forward (Tower a))) -> AD s (On (Forward (Tower a)))) -> a -> [a]
extremumNoEq f = Rank1.extremumNoEq (runAD.f.AD)
{-# INLINE extremumNoEq #-}
