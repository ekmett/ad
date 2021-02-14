{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
---- |
---- Copyright   :  (c) Edward Kmett 2010-2021
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

newtype AD s a = AD { runAD :: a }
  deriving (Eq,Ord,Show,Read,Bounded,Num,Real,Fractional,Floating,Enum,RealFrac,RealFloat,Erf,InvErf,Typeable)

instance Mode a => Mode (AD s a) where
  type Scalar (AD s a) = Scalar a
  isKnownConstant = isKnownConstant . runAD
  asKnownConstant = asKnownConstant . runAD
  isKnownZero = isKnownZero . runAD
  zero = AD zero
  auto = AD . auto
  AD a ^* b = AD (a ^* b)
  a *^ AD b = AD (a *^ b)
  AD a ^/ b = AD (a ^/ b)
