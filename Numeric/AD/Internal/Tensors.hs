{-# LANGUAGE TypeOperators, TemplateHaskell, ScopedTypeVariables #-}
-- {-# OPTIONS_HADDOCK hide #-}
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
--import Data.Data
--import Data.Typeable
import Numeric.AD.Internal.Comonad
import Numeric.AD.Internal.Stream

infixl 3 :-

data Tensors f a = a :- Tensors f (f a)
-- TODO: deriving (Data, Typeable)

instance Functor f => Functor (Tensors f) where
    fmap f (a :- as) = f a :- fmap (fmap f) as

instance Foldable f => Foldable (Tensors f) where
    foldMap f (a :- as) = f a `mappend` foldMap (foldMap f) as

instance Traversable f => Traversable (Tensors f) where
    traverse f (a :- as) = (:-) <$> f a <*> traverse (traverse f) as

-- | While we can not be a 'Comonad' without a 'fzip'-like operation, you can use the
-- comonad for @'Stream' f a@ to manipulate a structure comonadically that you can turn 
-- into 'Tensors'.
instance Functor f => Copointed (Tensors f) where
    extract (a :- _) = a

tailT :: Tensors f a -> Tensors f (f a)
tailT (_ :- as) = as
{-# INLINE tailT #-}

headT :: Tensors f a -> a
headT (a :- _) = a
{-# INLINE headT #-}

tensors :: Functor f => Stream f a -> Tensors f a
tensors (a :< as) = a :- distribute (tensors <$> as)
    where
        distribute :: Functor f => f (Tensors f a) -> Tensors f (f a)
        distribute x = (headT <$> x) :- distribute (tailT <$> x)
