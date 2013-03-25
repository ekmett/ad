{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable, TypeFamilies #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Internal.Identity
-- Copyright   :  (c) Edward Kmett 2010
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
    (Iso a, Eq, Ord, Show, Enum, Bounded, Num, Real, Fractional, Floating, RealFrac, RealFloat, Monoid, Data, Typeable, Erf, InvErf)

type instance Scalar (Id a s) = a

probe :: a -> Id a s
probe = Id

unprobe :: Id a s -> a
unprobe = runId

pid :: f a -> f (Id a s)
pid = iso

unpid :: f (Id a s) -> f a
unpid = osi

probed :: f a -> f (Id a s)
probed = pid

unprobed :: f (Id a s) -> f a
unprobed = unpid

instance Mode (Id a) s where
    auto = Id
    Id a ^* b = Id (a * b)
    a *^ Id b = Id (a * b)
    Id a <+> Id b = Id (a + b)
    Id a <**> Id b = Id (a ** b)

instance Primal (Id a s) where
    primal (Id a) = a

-- instance Erf a => Erf (Id a) where
--   erf = Id . erf . runId
--   erfc = Id . erfc . runId
--   normcdf = Id . normcdf . runId

-- instance InvErf a => InvErf (Id a) where
--   inverf = Id . inverf . runId
--   inverfc = Id . inverfc . runId
--   invnormcdf = Id . invnormcdf . runId
