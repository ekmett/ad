{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
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

module Numeric.AD.Internal.Classes
  (
  -- * AD modes
    Mode(..)
  , one
  , negOne
  -- * Automatically Deriving AD
  , Jacobian(..)
  -- , deriveNumeric
  , Scalar
  , withPrimal
  , fromBy
  ) where

type family Scalar t

infixr 6 <+>
infixr 7 *^
infixl 7 ^*
infixr 7 ^/

class (Num (Scalar t)) => Mode t where
  -- | allowed to return False for items with a zero derivative, but we'll give more NaNs than strictly necessary
  isKnownConstant :: t -> Bool
  isKnownConstant _ = False

  -- | allowed to return False for zero, but we give more NaN's than strictly necessary then
  isKnownZero :: t -> Bool
  isKnownZero _ = False

  -- | Embed a constant
  auto  :: Scalar t -> t

  -- | Vector sum
  (<+>) :: Num (Scalar t) => t -> t -> t

  -- | Scalar-vector multiplication
  (*^) :: Scalar t -> t -> t

  -- | Vector-scalar multiplication
  (^*) :: t -> Scalar t -> t

  -- | Scalar division
  (^/) :: (Num t, Fractional (Scalar t)) => t -> Scalar t -> t

  -- |
  -- @'zero' = 'lift' 0@
  zero :: t

#ifndef HLINT
  default (*^) :: Num t => Scalar t -> t -> t
  a *^ b = auto a * b
  default (^*) :: Num t => t -> Scalar t -> t
  a ^* b = a * auto b
#endif

  a ^/ b = a ^* recip b

  zero = auto 0

one :: Mode t => t
one = auto 1
{-# INLINE one #-}

negOne :: Mode t => t
negOne = auto (-1)
{-# INLINE negOne #-}

-- | 'Jacobian' is used by 'deriveMode' but is not exposed
-- via 'Mode' to prevent its abuse by end users
-- via the 'AD' data type.
class (Mode t, Mode (D t), Num (D t)) => Jacobian t where
  type D t :: *

  unary  :: (Scalar t -> Scalar t) -> D t -> t -> t
  lift1  :: (Scalar t -> Scalar t) -> (D t -> D t) -> t -> t
  lift1_ :: (Scalar t -> Scalar t) -> (D t -> D t -> D t) -> t -> t

  binary :: (Scalar t -> Scalar t -> Scalar t) -> D t -> D t -> t -> t -> t
  lift2  :: (Scalar t -> Scalar t -> Scalar t) -> (D t -> D t -> (D t, D t)) -> t -> t -> t
  lift2_ :: (Scalar t -> Scalar t -> Scalar t) -> (D t -> D t -> D t -> (D t, D t)) -> t -> t -> t

withPrimal :: (Jacobian t, Scalar t ~ Scalar (D t)) => t -> Scalar t -> t
withPrimal t a = unary (const a) one t
{-# INLINE withPrimal #-}

fromBy :: (Jacobian t, Scalar t ~ Scalar (D t)) => t -> t -> Int -> Scalar t -> t
fromBy a delta n x = binary (\_ _ -> x) one (fromIntegral n) a delta
