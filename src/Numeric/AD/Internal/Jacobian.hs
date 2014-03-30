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

module Numeric.AD.Internal.Jacobian
  ( Jacobian(..)
  , withPrimal
  , fromBy
  ) where

import Numeric.AD.Mode

-- | 'Jacobian' is useful for defining new AD primitives in a
-- fairly generic way.
class (Mode t, Mode (D t), Num (D t)) => Jacobian t where
  type D t :: *

  unary  :: (Scalar t -> Scalar t) -> D t -> t -> t
  lift1  :: (Scalar t -> Scalar t) -> (D t -> D t) -> t -> t
  lift1_ :: (Scalar t -> Scalar t) -> (D t -> D t -> D t) -> t -> t

  binary :: (Scalar t -> Scalar t -> Scalar t) -> D t -> D t -> t -> t -> t
  lift2  :: (Scalar t -> Scalar t -> Scalar t) -> (D t -> D t -> (D t, D t)) -> t -> t -> t
  lift2_ :: (Scalar t -> Scalar t -> Scalar t) -> (D t -> D t -> D t -> (D t, D t)) -> t -> t -> t

-- | Used internally to define various 'Enum' combinators.
withPrimal :: Jacobian t => t -> Scalar t -> t
withPrimal t a = unary (const a) one t
{-# INLINE withPrimal #-}

-- | Used internally to define various 'Enum' combinators.
fromBy :: Jacobian t => t -> t -> Int -> Scalar t -> t
fromBy a delta n x = binary (\_ _ -> x) one (fromIntegral n) a delta
