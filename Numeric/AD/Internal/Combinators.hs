-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Internal.Combinators
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------
module Numeric.AD.Internal.Combinators
    ( zipWithT
    , zipWithDefaultT
    , on
    ) where

import Data.Traversable (Traversable, mapAccumL)
import Data.Foldable (Foldable, toList)

on :: (a -> a -> b) -> (c -> a) -> c -> c -> b
on f g a b = f (g a) (g b)

zipWithT :: (Foldable f, Traversable g) => (a -> b -> c) -> f a -> g b -> g c
zipWithT f as = snd . mapAccumL (\(a:as') b -> (as', f a b)) (toList as)

zipWithDefaultT :: (Foldable f, Traversable g) => a -> (a -> b -> c) -> f a -> g b -> g c
zipWithDefaultT z f as = zipWithT f (toList as ++ repeat z)
