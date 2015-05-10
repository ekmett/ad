{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_HADDOCK not-home #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2010-2015
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
  , withPrimal
  , fromBy
  ) where

import Data.Traversable (Traversable, mapAccumL)
import Data.Foldable (Foldable, toList)
import Numeric.AD.Mode
import Numeric.AD.Jacobian

-- | Zip a @'Foldable' f@ with a @'Traversable' g@ assuming @f@ has at least as many entries as @g@.
zipWithT :: (Foldable f, Traversable g) => (a -> b -> c) -> f a -> g b -> g c
zipWithT f as = snd . mapAccumL (\(a:as') b -> (as', f a b)) (toList as)

-- | Zip a @'Foldable' f@ with a @'Traversable' g@ assuming @f@, using a default value after @f@ is exhausted.
zipWithDefaultT :: (Foldable f, Traversable g) => a -> (a -> b -> c) -> f a -> g b -> g c
zipWithDefaultT z f as = zipWithT f (toList as ++ repeat z)

-- | Used internally to define various 'Enum' combinators.
withPrimal :: Jacobian t => t -> Scalar t -> t
withPrimal t a = unary (const a) 1 t
{-# INLINE withPrimal #-}

-- | Used internally to define various 'Enum' combinators.
fromBy :: Jacobian t => t -> t -> Int -> Scalar t -> t
fromBy a delta n x = binary (\_ _ -> x) 1 (fromIntegral n) a delta
