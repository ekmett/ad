{-# LANGUAGE Rank2Types, TemplateHaskell, BangPatterns, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Variadic
-- Copyright   :  (c) Edward Kmett 2010-2012
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Variadic combinators for variadic mixed-mode automatic differentiation.
--
-- Unfortunately, variadicity comes at the expense of being able to use
-- quantification to avoid sensitivity confusion, so be careful when
-- counting the number of @lift@ you use when taking the gradient of a
-- function that takes gradients!
--
-----------------------------------------------------------------------------

module Numeric.AD.Variadic
    (
    -- * Reverse-mode variadic gradient
      Grad , vgrad, vgrad'
    -- * Sparse forward mode variadic jet
    , Grads, vgrads
    ) where

import Numeric.AD.Variadic.Reverse
import Numeric.AD.Variadic.Sparse (Grads, vgrads)
