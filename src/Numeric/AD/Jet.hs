{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2010-2021
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------
module Numeric.AD.Jet
  ( Jet(..)
  , headJet
  , tailJet
  , jet
  , unjet
  ) where

import Data.Functor.Rep
import Data.Typeable
import Control.Comonad.Cofree

infixr 3 :-

-- | A 'Jet' is a tower of all (higher order) partial derivatives of a function
--
-- At each step, a @'Jet' f@ is wrapped in another layer worth of @f@.
--
-- > a :- f a :- f (f a) :- f (f (f a)) :- ...
data Jet f a = a :- Jet f (f a)
  deriving Typeable

-- | Used to sidestep the need for UndecidableInstances.
newtype Showable = Showable (Int -> String -> String)

instance Show Showable where
  showsPrec d (Showable f) = f d

showable :: Show a => a -> Showable
showable a = Showable (`showsPrec` a)

-- Polymorphic recursion precludes 'Data' in its current form, as no Data1 class exists
-- Polymorphic recursion also breaks 'show' for 'Jet'!
-- factor Show1 out of Lifted?
instance (Functor f, Show (f Showable), Show a) => Show (Jet f a) where
  showsPrec d (a :- as) = showParen (d > 3) $
    showsPrec 4 a . showString " :- " . showsPrec 3 (fmap showable <$> as)

instance Functor f => Functor (Jet f) where
  fmap f (a :- as) = f a :- fmap (fmap f) as

instance Foldable f => Foldable (Jet f) where
  foldMap f (a :- as) = f a `mappend` foldMap (foldMap f) as

instance Traversable f => Traversable (Jet f) where
  traverse f (a :- as) = (:-) <$> f a <*> traverse (traverse f) as

-- | Take the tail of a 'Jet'.
tailJet :: Jet f a -> Jet f (f a)
tailJet (_ :- as) = as
{-# INLINE tailJet #-}

-- | Take the head of a 'Jet'.
headJet :: Jet f a -> a
headJet (a :- _) = a
{-# INLINE headJet #-}

-- | Construct a 'Jet' by unzipping the layers of a 'Cofree' 'Comonad'.
jet :: Functor f => Cofree f a -> Jet f a
jet (a :< as) = a :- dist (jet <$> as) where
  dist :: Functor f => f (Jet f a) -> Jet f (f a)
  dist x = (headJet <$> x) :- dist (tailJet <$> x)

unjet :: Representable f => Jet f a -> Cofree f a
unjet (a :- as) = a :< (unjet <$> undist as) where
  undist :: Representable f => Jet f (f a) -> f (Jet f a)
  undist (fa :- fas) = tabulate $ \i -> index fa i :- index (undist fas) i

