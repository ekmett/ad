{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
---- |
---- Copyright   :  (c) Edward Kmett 2010-2014
---- License     :  BSD3
---- Maintainer  :  ekmett@gmail.com
---- Stability   :  experimental
---- Portability :  GHC only
----
-------------------------------------------------------------------------------
module Numeric.AD.Internal.Type
  ( AD(..)
  ) where

import Data.Number.Erf
import Numeric.AD.Mode
import Data.Typeable

newtype AD a s = AD { runAD :: a }
  deriving (Eq,Ord,Show,Read,Bounded,Num,Real,Fractional,Floating,Enum,RealFrac,RealFloat,Erf,InvErf,Typable)

instance Mode a => Mode (AD a s) where
  type Scalar (AD a s) = Scalar a
  isKnownConstant = isKnownConstant . runAD
  isKnownZero = isKnownZero . runAD
  zero = AD zero
  auto = AD . auto
  AD a ^* b = AD (a ^* b)
  a *^ AD b = AD (a *^ b)
  AD a ^/ b = AD (a ^/ b)
