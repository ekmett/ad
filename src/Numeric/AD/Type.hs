{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Numeric.AD.Type
  ( AD(..)
  ) where

import Data.Number.Erf
import Numeric.AD.Mode

newtype AD a s = AD { runAD :: a }
  deriving (Eq,Ord,Show,Read,Bounded,Num,Real,Fractional,Floating,Enum,RealFrac,RealFloat,Erf,InvErf)

instance Mode a => Mode (AD a s) where
  type Scalar (AD a s) = Scalar a
  isKnownConstant = isKnownConstant . runAD
  isKnownZero = isKnownZero . runAD
  zero = AD zero
  auto = AD . auto
  AD a ^* b = AD (a ^* b)
  a *^ AD b = AD (a *^ b)
  AD a ^/ b = AD (a ^/ b)
