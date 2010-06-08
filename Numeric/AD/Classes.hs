-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Classes
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------

module Numeric.AD.Classes
    ( 
    -- * AD Modes
      Mode(..)
    -- * Comonads
    , Copointed(..)
    , Comonad(..)
    ) where

import Numeric.AD.Internal.Classes
import Numeric.AD.Internal.Comonad
