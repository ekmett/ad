{-# LANGUAGE TypeOperators, TemplateHaskell, ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Internal.Comonad
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------

-- TODO: separate a \"comonads\" package from \"category-extras\"

module Numeric.AD.Internal.Comonad
    ( Copointed(..)
    , Comonad(..)
    ) where

class Functor f => Copointed f where
    extract :: f a -> a

class Copointed f => Comonad f where
    duplicate :: f a -> f (f a)
    extend :: (f a -> b) -> f a -> f b

    duplicate = extend id
    extend f = fmap f . duplicate
