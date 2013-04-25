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

newtype Id s a = Id { runId :: a } deriving
    (Iso a, Eq, Ord, Show, Enum, Bounded, Num, Real, Fractional, Floating, RealFrac, RealFloat, Monoid, Data, Typeable, Erf, InvErf)

type instance Scalar (Id s a) = a

probe :: a -> Id s a
probe = Id

unprobe :: Id s a -> a
unprobe = runId

pid :: f a -> f (Id s a)
pid = iso

unpid :: f (Id s a) -> f a
unpid = osi

probed :: f a -> f (Id s a)
probed = pid

unprobed :: f (Id s a) -> f a
unprobed = unpid

instance Num a => Mode (Id s a) where
    auto = Id
    Id a ^* b = Id (a * b)
    a *^ Id b = Id (a * b)
    Id a <+> Id b = Id (a + b)
    Id a <**> Id b = Id (a ** b)

instance Num a => Primal (Id s a) where
    primal (Id a) = a

-- instance Erf a => Erf (Id a) where
--   erf = Id . erf . runId
--   erfc = Id . erfc . runId
--   normcdf = Id . normcdf . runId

-- instance InvErf a => InvErf (Id a) where
--   inverf = Id . inverf . runId
--   inverfc = Id . inverfc . runId
--   invnormcdf = Id . invnormcdf . runId
