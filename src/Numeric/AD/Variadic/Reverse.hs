{-# LANGUAGE Rank2Types, TemplateHaskell, BangPatterns, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Variadic.Reverse
-- Copyright   :  (c) Edward Kmett 2010-2012
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Variadic combinators for reverse-mode automatic differentiation.
--
-- Unfortunately, variadicity comes at the expense of being able to use
-- quantification to avoid sensitivity confusion, so be careful when
-- counting the number of @lift@ you use when taking the gradient of a
-- function that takes gradients!
--
-----------------------------------------------------------------------------

module Numeric.AD.Variadic.Reverse
    (
    -- * Unsafe Variadic Gradient
      vgrad, vgrad'
    , Grad
    ) where

import Numeric.AD.Internal.Reverse
