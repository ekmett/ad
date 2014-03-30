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

module Numeric.AD.Mode
  (
  -- * AD modes
    Mode(..)
  , one
  , negOne
  , Scalar
  ) where

type family Scalar (t :: *) :: *

infixr 6 <+>
infixr 7 *^
infixl 7 ^*
infixr 7 ^/

class Num (Scalar t) => Mode t where
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
