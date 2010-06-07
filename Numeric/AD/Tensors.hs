{-# LANGUAGE TypeOperators, TemplateHaskell, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Tensors
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------

module Numeric.AD.Tensors
    ( 
    -- * Tensors
      Tensors(..)
    , headT
    , tailT
    , tensors
    -- * f-Branching Streams
    , Stream(..)
    , headS
    , tailS
    , unfoldS
    -- * Comonads
    , Copointed(..)
    , Comonad(..)
    ) where

import Numeric.AD.Internal.Comonad
import Numeric.AD.Internal.Stream
import Numeric.AD.Internal.Tensors
