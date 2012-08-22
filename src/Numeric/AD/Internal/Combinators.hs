{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Internal.Combinators
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Combinators used internally by @Numeric.AD@
-----------------------------------------------------------------------------
module Numeric.AD.Internal.Combinators
    ( zipWithT
    , zipWithDefaultT
    ) where

import Data.Traversable (Traversable, mapAccumL)
import Data.Foldable (Foldable, toList)

-- | Zip a @'Foldable' f@ with a @'Traversable' g@ assuming @f@ has at least as many entries as @g@.
zipWithT :: (Foldable f, Traversable g) => (a -> b -> c) -> f a -> g b -> g c
zipWithT f as = snd . mapAccumL (\(a:as') b -> (as', f a b)) (toList as)

-- | Zip a @'Foldable' f@ with a @'Traversable' g@ assuming @f@, using a default value after @f@ is exhausted.
zipWithDefaultT :: (Foldable f, Traversable g) => a -> (a -> b -> c) -> f a -> g b -> g c
zipWithDefaultT z f as = zipWithT f (toList as ++ repeat z)
