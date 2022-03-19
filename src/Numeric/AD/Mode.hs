{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2010-2021
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
  , pattern KnownZero
  , pattern Auto
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
  type Scalar t = t

  -- | allowed to return False for items with a zero derivative, but we'll give more NaNs than strictly necessary
  isKnownConstant :: t -> Bool
  isKnownConstant _ = False

  asKnownConstant :: t -> Maybe (Scalar t)
  asKnownConstant _ = Nothing

  -- | allowed to return False for zero, but we give more NaN's than strictly necessary
  isKnownZero :: t -> Bool
  isKnownZero _ = False

  -- | Embed a constant
  auto  :: Scalar t -> t
  default auto :: (Scalar t ~ t) => Scalar t -> t
  auto = id

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

pattern KnownZero :: Mode s => s
pattern KnownZero <- (isKnownZero -> True) where
  KnownZero = zero

pattern Auto :: Mode s => Scalar s -> s
pattern Auto n <- (asKnownConstant -> Just n) where
  Auto n = auto n

instance Mode Double where
  isKnownConstant _ = True
  asKnownConstant = Just
  isKnownZero x = 0 == x
  (^/) = (/)

instance Mode Float where
  isKnownConstant _ = True
  asKnownConstant = Just
  isKnownZero x = 0 == x
  (^/) = (/)

instance Mode Int where
  isKnownConstant _ = True
  asKnownConstant = Just
  isKnownZero x = 0 == x
  (^/) = (/)

instance Mode Integer where
  isKnownConstant _ = True
  asKnownConstant = Just
  isKnownZero x = 0 == x
  (^/) = (/)

instance Mode Int8 where
  isKnownConstant _ = True
  asKnownConstant = Just
  isKnownZero x = 0 == x
  (^/) = (/)

instance Mode Int16 where
  isKnownConstant _ = True
  asKnownConstant = Just
  isKnownZero x = 0 == x
  (^/) = (/)

instance Mode Int32 where
  isKnownConstant _ = True
  asKnownConstant = Just
  isKnownZero x = 0 == x
  (^/) = (/)

instance Mode Int64 where
  isKnownConstant _ = True
  asKnownConstant = Just
  isKnownZero x = 0 == x
  (^/) = (/)

instance Mode Natural where
  isKnownConstant _ = True
  asKnownConstant = Just
  isKnownZero x = 0 == x
  (^/) = (/)

instance Mode Word where
  isKnownConstant _ = True
  asKnownConstant = Just
  isKnownZero x = 0 == x
  (^/) = (/)

instance Mode Word8 where
  isKnownConstant _ = True
  asKnownConstant = Just
  isKnownZero x = 0 == x
  (^/) = (/)

instance Mode Word16 where
  isKnownConstant _ = True
  asKnownConstant = Just
  isKnownZero x = 0 == x
  (^/) = (/)

instance Mode Word32 where
  isKnownConstant _ = True
  asKnownConstant = Just
  isKnownZero x = 0 == x
  (^/) = (/)

instance Mode Word64 where
  isKnownConstant _ = True
  asKnownConstant = Just
  isKnownZero x = 0 == x
  (^/) = (/)

instance RealFloat a => Mode (Complex a) where
  isKnownConstant _ = True
  asKnownConstant = Just
  isKnownZero x = 0 == x
  (^/) = (/)

instance Integral a => Mode (Ratio a) where
  isKnownConstant _ = True
  asKnownConstant = Just
  isKnownZero x = 0 == x
  (^/) = (/)
