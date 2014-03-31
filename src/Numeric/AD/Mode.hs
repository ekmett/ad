{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2010-2014
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------

module Numeric.AD.Mode
  (
  -- * AD modes
    Mode(..)
  ) where

import Numeric.Natural
import Data.Complex
import Data.Int
import Data.Ratio
import Data.Word

infixr 7 *^
infixl 7 ^*
infixr 7 ^/

class (Num t, Num (Scalar t)) => Mode t where
  type Scalar t
  -- | allowed to return False for items with a zero derivative, but we'll give more NaNs than strictly necessary
  isKnownConstant :: t -> Bool
  isKnownConstant _ = False

  -- | allowed to return False for zero, but we give more NaN's than strictly necessary then
  isKnownZero :: t -> Bool
  isKnownZero _ = False

  -- | Embed a constant
  auto  :: Scalar t -> t

  -- | Scalar-vector multiplication
  (*^) :: Scalar t -> t -> t
  a *^ b = auto a * b

  -- | Vector-scalar multiplication
  (^*) :: t -> Scalar t -> t
  a ^* b = a * auto b

  -- | Scalar division
  (^/) :: Fractional (Scalar t) => t -> Scalar t -> t
  a ^/ b = a ^* recip b

  -- |
  -- @'zero' = 'lift' 0@
  zero :: t
  zero = auto 0

instance Mode Double where
  type Scalar Double = Double
  isKnownConstant _ = True
  isKnownZero x = 0 == x
  auto = id
  (^/) = (/)

instance Mode Float where
  type Scalar Float = Float
  isKnownConstant _ = True
  isKnownZero x = 0 == x
  auto = id
  (^/) = (/)

instance Mode Int where
  type Scalar Int = Int
  isKnownConstant _ = True
  isKnownZero x = 0 == x
  auto = id
  (^/) = (/)

instance Mode Integer where
  type Scalar Integer = Integer
  isKnownConstant _ = True
  isKnownZero x = 0 == x
  auto = id
  (^/) = (/)

instance Mode Int8 where
  type Scalar Int8 = Int8
  isKnownConstant _ = True
  isKnownZero x = 0 == x
  auto = id
  (^/) = (/)

instance Mode Int16 where
  type Scalar Int16 = Int16
  isKnownConstant _ = True
  isKnownZero x = 0 == x
  auto = id
  (^/) = (/)

instance Mode Int32 where
  type Scalar Int32 = Int32
  isKnownConstant _ = True
  isKnownZero x = 0 == x
  auto = id
  (^/) = (/)

instance Mode Int64 where
  type Scalar Int64 = Int64
  isKnownConstant _ = True
  isKnownZero x = 0 == x
  auto = id
  (^/) = (/)

instance Mode Natural where
  type Scalar Natural = Natural
  isKnownConstant _ = True
  isKnownZero x = 0 == x
  auto = id
  (^/) = (/)

instance Mode Word where
  type Scalar Word = Word
  isKnownConstant _ = True
  isKnownZero x = 0 == x
  auto = id
  (^/) = (/)

instance Mode Word8 where
  type Scalar Word8 = Word8
  isKnownConstant _ = True
  isKnownZero x = 0 == x
  auto = id
  (^/) = (/)

instance Mode Word16 where
  type Scalar Word16 = Word16
  isKnownConstant _ = True
  isKnownZero x = 0 == x
  auto = id
  (^/) = (/)

instance Mode Word32 where
  type Scalar Word32 = Word32
  isKnownConstant _ = True
  isKnownZero x = 0 == x
  auto = id
  (^/) = (/)

instance Mode Word64 where
  type Scalar Word64 = Word64
  isKnownConstant _ = True
  isKnownZero x = 0 == x
  auto = id
  (^/) = (/)

instance RealFloat a => Mode (Complex a) where
  type Scalar (Complex a) = Complex a
  isKnownConstant _ = True
  isKnownZero x = 0 == x
  auto = id
  (^/) = (/)

instance Integral a => Mode (Ratio a) where
  type Scalar (Ratio a) = Ratio a
  isKnownConstant _ = True
  isKnownZero x = 0 == x
  auto = id
  (^/) = (/)
