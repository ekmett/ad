{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Internal.Stream
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------

module Numeric.AD.Internal.Stream 
    ( Stream(..)
    , unfoldS
    , headS
    , tailS
    ) where

import Control.Applicative
import Data.Monoid
import Data.Foldable
import Data.Traversable
import Data.Data (Data(..), mkDataType, DataType, mkConstr, Constr, constrIndex, Fixity(Infix))
import Data.Typeable (Typeable1(..), TyCon, mkTyCon, mkTyConApp, gcast1)
import Numeric.AD.Internal.Comonad

infixl 3 :<

data Stream f a = a :< f (Stream f a)

deriving instance (Show a, Show (f (Stream f a))) => Show (Stream f a)

instance Functor f => Functor (Stream f) where
    fmap f (a :< as) = f a :< fmap f <$> as

instance Functor f => Copointed (Stream f) where
    extract (a :< _) = a

instance Functor f => Comonad (Stream f) where
    duplicate aas@(_ :< as) = aas :< duplicate <$> as
    extend f aas@(_ :< as) = f aas :< extend f <$> as

instance Foldable f => Foldable (Stream f) where
    foldMap f (a :< as) = f a `mappend` foldMap (foldMap f) as

instance Traversable f => Traversable (Stream f) where
    traverse f (a :< as) = (:<) <$> f a <*> traverse (traverse f) as

headS :: Stream f a -> a
headS (a :< _) = a
{-# INLINE headS #-}

-- tails of the f-branching stream comonad/cofree comonad
tailS :: Stream f a -> f (Stream f a)
tailS (_ :< as) = as
{-# INLINE tailS #-}


unfoldS :: Functor f => (a -> (b, f a)) -> a -> Stream f b
unfoldS f a = h :< unfoldS f <$> t 
    where
        (h, t) = f a

instance Typeable1 f => Typeable1 (Stream f) where
    typeOf1 tfa = mkTyConApp streamTyCon [typeOf1 (undefined `asArgsType` tfa)]
        where asArgsType :: f a -> t f a -> f a
              asArgsType = const

streamTyCon :: TyCon
streamTyCon = mkTyCon "Numeric.AD.Internal.Stream.Stream"
{-# NOINLINE streamTyCon #-}

consConstr :: Constr
consConstr = mkConstr streamDataType "(:<)" [] Infix
{-# NOINLINE consConstr #-}

streamDataType :: DataType
streamDataType = mkDataType "Numeric.AD.Internal.Stream.Stream" [consConstr]
{-# NOINLINE streamDataType #-}

instance (Typeable1 f, Data (f (Stream f a)), Data a) => Data (Stream f a) where
    gfoldl f z (a :< as) = z (:<) `f` a `f` as
    toConstr _ = consConstr
    gunfold k z c = case constrIndex c of
        1 -> k (k (z (:<)))
        _ -> error "gunfold"
    dataTypeOf _ = streamDataType
    dataCast1 f = gcast1 f

