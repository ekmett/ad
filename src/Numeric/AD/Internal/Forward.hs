{-# LANGUAGE Rank2Types, TypeFamilies, DeriveDataTypeable, TemplateHaskell, UndecidableInstances, BangPatterns #-}
-- {-# OPTIONS_HADDOCK hide, prune #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Internal.Forward
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Unsafe and often partial combinators intended for internal usage.
--
-- Handle with care.
-----------------------------------------------------------------------------

module Numeric.AD.Internal.Forward
    ( Forward(..)
    , tangent
    , bundle
    , unbundle
    , apply
    , bind
    , bind'
    , bindWith
    , bindWith'
    , transposeWith
    ) where

import Language.Haskell.TH
import Data.Typeable
import Data.Traversable (Traversable, mapAccumL)
import Data.Foldable (Foldable, toList)
import Data.Data
import Control.Applicative
import Numeric.AD.Internal.Types
import Numeric.AD.Internal.Classes
import Numeric.AD.Internal.Identity

-- | 'Forward' mode AD.
data Forward a
  = Forward !a a
  | Lift !a
  | Zero
  deriving (Show, Data, Typeable)

-- | Calculate the 'tangent' using forward mode AD.
tangent :: Num a => AD Forward a -> a
tangent (AD (Forward _ da)) = da
tangent _ = 0
{-# INLINE tangent #-}

unbundle :: Num a => AD Forward a -> (a, a)
unbundle (AD (Forward a da)) = (a, da)
unbundle (AD Zero) = (0,0)
unbundle (AD (Lift a)) = (a, 0)
{-# INLINE unbundle #-}

bundle :: a -> a -> AD Forward a
bundle a da = AD (Forward a da)
{-# INLINE bundle #-}

apply :: Num a => (AD Forward a -> b) -> a -> b
apply f a = f (bundle a 1)
{-# INLINE apply #-}

instance Primal Forward where
    primal (Forward a _) = a
    primal (Lift a) = a
    primal Zero = 0

instance Lifted Forward => Mode Forward where
    lift = Lift
    zero = Zero

    isKnownZero Zero = True
    isKnownZero _    = False

    isKnownConstant Forward{} = False
    isKnownConstant _ = True

    Zero <+> a = a
    a <+> Zero = a
    Forward a da <+> Forward b db = Forward (a + b) (da + db)
    Forward a da <+> Lift b = Forward (a + b) da
    Lift a <+> Forward b db = Forward (a + b) db
    Lift a <+> Lift b = Lift (a + b)

    Zero <**> y      = lift (0 ** primal y)
    _    <**> Zero   = lift 1
    x    <**> Lift y = lift1 (**y) (\z -> (y *^ z ** Id (y-1))) x
    x    <**> y      = lift2_ (**) (\z xi yi -> (yi *! z /! xi, z *! log1 xi)) x y

    a *^ Forward b db = Forward (a * b) (a * db)
    a *^ Lift b = Lift (a * b)
    _ *^ Zero = Zero

    Forward a da ^* b = Forward (a * b) (da * b)
    Lift a ^* b = Lift (a * b)
    Zero ^* _ = Zero

    Forward a da ^/ b = Forward (a / b) (da / b)
    Lift a ^/ b = Lift (a / b)
    Zero ^/ _ = Zero

instance Lifted Forward => Jacobian Forward where
    type D Forward = Id


    unary f (Id dadb) (Forward b db) = Forward (f b) (dadb * db)
    unary f _         (Lift b)       = Lift (f b)
    unary f _         Zero           = Lift (f 0)

    lift1 f _ Zero            = Lift (f 0)
    lift1 f _  (Lift b)       = Lift (f b)
    lift1 f df (Forward b db) = Forward (f b) (dadb * db)
        where
            Id dadb = df (Id b)

    lift1_ f _  Zero           = Lift (f 0)
    lift1_ f _  (Lift b)       = Lift (f b)
    lift1_ f df (Forward b db) = Forward a da
        where
            a = f b
            Id da = df (Id a) (Id b) ^* db

    binary f _         _         Zero           Zero           = Lift (f 0 0)
    binary f _         _         Zero           (Lift c)       = Lift (f 0 c)
    binary f _         _         (Lift b)       Zero           = Lift (f b 0)
    binary f _         _         (Lift b)       (Lift c)       = Lift (f b c)
    binary f _         (Id dadc) Zero           (Forward c dc) = Forward (f 0 c) $ dc * dadc
    binary f _         (Id dadc) (Lift b)       (Forward c dc) = Forward (f b c) $ dc * dadc
    binary f (Id dadb) _         (Forward b db) Zero           = Forward (f b 0) $ dadb * db
    binary f (Id dadb) _         (Forward b db) (Lift c)       = Forward (f b c) $ dadb * db
    binary f (Id dadb) (Id dadc) (Forward b db) (Forward c dc) = Forward (f b c) $ dadb * db + dc * dadc

    lift2 f _  Zero           Zero           = Lift (f 0 0)
    lift2 f _  Zero           (Lift c)       = Lift (f 0 c)
    lift2 f _  (Lift b)       Zero           = Lift (f b 0)
    lift2 f _  (Lift b)       (Lift c)       = Lift (f b c)
    lift2 f df Zero           (Forward c dc) = Forward (f 0 c) $ dc * runId (snd (df (Id 0) (Id c)))
    lift2 f df (Lift b)       (Forward c dc) = Forward (f b c) $ dc * runId (snd (df (Id b) (Id c)))
    lift2 f df (Forward b db) Zero           = Forward (f b 0) $ runId (fst (df (Id b) (Id 0))) * db
    lift2 f df (Forward b db) (Lift c)       = Forward (f b c) $ runId (fst (df (Id b) (Id c))) * db
    lift2 f df (Forward b db) (Forward c dc) = Forward a da
        where
            a = f b c
            (Id dadb, Id dadc) = df (Id b) (Id c)
            da = dadb * db + dc * dadc

    lift2_ f _  Zero           Zero           = Lift (f 0 0)
    lift2_ f _  Zero           (Lift c)       = Lift (f 0 c)
    lift2_ f _  (Lift b)       Zero           = Lift (f b 0)
    lift2_ f _  (Lift b)       (Lift c)       = Lift (f b c)
    lift2_ f df Zero           (Forward c dc) = Forward a $ dc * runId (snd (df (Id a) (Id 0) (Id c))) where a = f 0 c
    lift2_ f df (Lift b)       (Forward c dc) = Forward a $ dc * runId (snd (df (Id a) (Id b) (Id c))) where a = f b c
    lift2_ f df (Forward b db) Zero           = Forward a $ runId (fst (df (Id a) (Id b) (Id 0))) * db where a = f b 0
    lift2_ f df (Forward b db) (Lift c)       = Forward a $ runId (fst (df (Id a) (Id b) (Id c))) * db where a = f b c
    lift2_ f df (Forward b db) (Forward c dc) = Forward a da
        where
            a = f b c
            (Id dadb, Id dadc) = df (Id a) (Id b) (Id c)
            da = dadb * db + dc * dadc

deriveLifted id $ conT ''Forward

bind :: (Traversable f, Num a) => (f (AD Forward a) -> b) -> f a -> f b
bind f as = snd $ mapAccumL outer (0 :: Int) as
    where
        outer !i _ = (i + 1, f $ snd $ mapAccumL (inner i) 0 as)
        inner !i !j a = (j + 1, if i == j then bundle a 1 else AD Zero)

bind' :: (Traversable f, Num a) => (f (AD Forward a) -> b) -> f a -> (b, f b)
bind' f as = dropIx $ mapAccumL outer (0 :: Int, b0) as
    where
        outer (!i, _) _ = let b = f $ snd $ mapAccumL (inner i) (0 :: Int) as in ((i + 1, b), b)
        inner !i !j a = (j + 1, if i == j then bundle a 1 else AD Zero)
        b0 = f (lift <$> as)
        dropIx ((_,b),bs) = (b,bs)

bindWith :: (Traversable f, Num a) => (a -> b -> c) -> (f (AD Forward a) -> b) -> f a -> f c
bindWith g f as = snd $ mapAccumL outer (0 :: Int) as
    where
        outer !i a = (i + 1, g a $ f $ snd $ mapAccumL (inner i) 0 as)
        inner !i !j a = (j + 1, if i == j then bundle a 1 else AD Zero)

bindWith' :: (Traversable f, Num a) => (a -> b -> c) -> (f (AD Forward a) -> b) -> f a -> (b, f c)
bindWith' g f as = dropIx $ mapAccumL outer (0 :: Int, b0) as
    where
        outer (!i, _) a = let b = f $ snd $ mapAccumL (inner i) (0 :: Int) as in ((i + 1, b), g a b)
        inner !i !j a = (j + 1, if i == j then bundle a 1 else AD Zero)
        b0 = f (lift <$> as)
        dropIx ((_,b),bs) = (b,bs)

-- we can't transpose arbitrary traversables, since we can't construct one out of whole cloth, and the outer
-- traversable could be empty. So instead we use one as a 'skeleton'
transposeWith :: (Functor f, Foldable f, Traversable g) => (b -> f a -> c) -> f (g a) -> g b -> g c
transposeWith f as = snd . mapAccumL go xss0
    where
        go xss b = (tail <$> xss, f b (head <$> xss))
        xss0 = toList <$> as

