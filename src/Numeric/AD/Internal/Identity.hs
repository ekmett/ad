{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2010-2021
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------
module Numeric.AD.Internal.Identity
  ( Id(..)
  , probe
  , unprobe
  , probed
  , unprobed
  ) where

import Data.Data (Data)
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif
import Data.Number.Erf
import Numeric.AD.Mode

newtype Id a = Id { runId :: a } deriving
  ( Eq, Ord, Show, Enum, Bounded
  , Num, Real, Fractional, Floating
  , RealFrac, RealFloat, Semigroup
  , Monoid, Data, Erf, InvErf
  )

probe :: a -> Id a
probe = Id

unprobe :: Id a -> a
unprobe = runId

pid :: Functor f => f a -> f (Id a)
pid = fmap probe

unpid :: Functor f => f (Id a) -> f a
unpid = fmap unprobe

probed :: Functor f => f a -> f (Id a)
probed = pid

unprobed :: Functor f => f (Id a) -> f a
unprobed = unpid

instance Num a => Mode (Id a) where
  type Scalar (Id a) = a
  auto = Id
  Id a ^* b = Id (a * b)
  a *^ Id b = Id (a * b)
