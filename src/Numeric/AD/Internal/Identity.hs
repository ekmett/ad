{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2010-2014
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
import Data.Monoid
import Data.Number.Erf
import Data.Typeable (Typeable)
import Numeric.AD.Internal.Classes

newtype Id a s = Id { runId :: a } deriving
  (Eq, Ord, Show, Enum, Bounded, Num, Real, Fractional, Floating, RealFrac, RealFloat, Monoid, Data, Typeable, Erf, InvErf)

type instance Scalar (Id a s) = a

probe :: a -> Id a s
probe = Id

unprobe :: Id a s -> a
unprobe = runId

pid :: Functor f => f a -> f (Id a s)
pid = fmap probe

unpid :: Functor f => f (Id a s) -> f a
unpid = fmap unprobe

probed :: Functor f => f a -> f (Id a s)
probed = pid

unprobed :: Functor f => f (Id a s) -> f a
unprobed = unpid

instance Num a => Mode (Id a s) where
  auto = Id
  Id a ^* b = Id (a * b)
  a *^ Id b = Id (a * b)
  Id a <+> Id b = Id (a + b)

instance Num a => Primal (Id a s) where
  primal (Id a) = a
