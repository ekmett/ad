{-# LANGUAGE TypeOperators, TemplateHaskell, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Stream
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- A cofree comonad/f-branching stream  for use in returning towers of gradients. 
--
-----------------------------------------------------------------------------

module Numeric.AD.Stream 
    ( (:>)(..)
    , Comonad(..)
    , unfold
    , tails
    ) where

import Numeric.AD.Internal.Stream
