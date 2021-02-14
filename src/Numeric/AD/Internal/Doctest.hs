{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2010-2021
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- A version of 'Double' that rounds to the twelfth digit if necessary. This
-- is useful for @ad@'s doctests since they must print out floating-point
-- numbers in their entirety, but the actual numbers that get produced can
-- vary slightly depending on machine-specific implementation details.
-- (See #73 for an example.) This works around the issue by just rounding
-- up the printed result to a point where it should be consistent across
-- all machines.
-----------------------------------------------------------------------------
module Numeric.AD.Internal.Doctest (RDouble) where

import GHC.Float
import Numeric

newtype RDouble = RDouble Double
  deriving (Floating, Fractional, Num)

instance Show RDouble where
  showsPrec p (RDouble d)
    | length is' >= limit
    = showSignedFloat (showGFloat (Just limit)) p d
    | otherwise
    = showsPrec p d
    where
      limit = 12
      (is, e) = floatToDigits 10 (abs d)
      is' = drop e is
