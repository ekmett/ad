-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Types
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------

module Numeric.AD.Types
    ( 
      AD(..)
    -- * Differentiable Functions
    , UU, UF, FU, FF
    -- * Tensors
    , Tensors(..)
    , headT
    , tailT
    , tensors
    -- * f-Branching Streams
    , Stream(..)
    , headS
    , tailS
    , unfoldS
    ) where

import Numeric.AD.Internal.Types
import Numeric.AD.Internal.Stream
import Numeric.AD.Internal.Tensors
