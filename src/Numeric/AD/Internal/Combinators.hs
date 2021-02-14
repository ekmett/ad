{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_HADDOCK not-home #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2010-2021
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
  , takeWhileDifferent
  ) where

import Data.Traversable (mapAccumL)
import Data.Foldable (toList)
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

-- | Used internally to implement functions which truncate lists after the
-- stream of results converge
takeWhileDifferent :: Eq a => [a] -> [a]
takeWhileDifferent (x1:x2:xs) = if x1 == x2
                                  then [x1]
                                  else x1 : takeWhileDifferent (x2:xs)
takeWhileDifferent xs = xs
