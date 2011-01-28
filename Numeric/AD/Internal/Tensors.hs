{-# LANGUAGE TypeOperators, TemplateHaskell, ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Internal.Tensors
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------

module Numeric.AD.Internal.Tensors
    ( Tensors(..)
    , headT
    , tailT
    , tensors
    ) where

import Control.Applicative
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Data.Typeable (Typeable1(..), TyCon, mkTyCon, mkTyConApp)
import Data.Stream.Branching as Branching

infixl 3 :-

-- Polymorphic recursion precludes 'Data' in its current form, as no Data1 class exists
-- Polymorphic recursion also breaks 'show' for 'Tensors'!
-- factor Show1 out of Lifted?
data Tensors f a = a :- Tensors f (f a)

instance Functor f => Functor (Tensors f) where
    fmap f (a :- as) = f a :- fmap (fmap f) as

instance Foldable f => Foldable (Tensors f) where
    foldMap f (a :- as) = f a `mappend` foldMap (foldMap f) as

instance Traversable f => Traversable (Tensors f) where
    traverse f (a :- as) = (:-) <$> f a <*> traverse (traverse f) as

tailT :: Tensors f a -> Tensors f (f a)
tailT (_ :- as) = as
{-# INLINE tailT #-}

headT :: Tensors f a -> a
headT (a :- _) = a
{-# INLINE headT #-}

tensors :: Functor f => Stream f a -> Tensors f a
tensors (a :< as) = a :- dist (tensors <$> as)
    where
        dist :: Functor f => f (Tensors f a) -> Tensors f (f a)
        dist x = (headT <$> x) :- dist (tailT <$> x)

instance Typeable1 f => Typeable1 (Tensors f) where
    typeOf1 tfa = mkTyConApp tensorsTyCon [typeOf1 (undefined `asArgsType` tfa)]
        where asArgsType :: f a -> t f a -> f a
              asArgsType = const

tensorsTyCon :: TyCon
tensorsTyCon = mkTyCon "Numeric.AD.Internal.Tensors.Tensors"
{-# NOINLINE tensorsTyCon #-}
