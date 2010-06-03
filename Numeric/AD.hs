{-# LANGUAGE Rank2Types, TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only 
--
-- Mixed-Mode Automatic Differentiation.
-- 
-- Each combinator exported from this module chooses an appropriate AD mode.
-----------------------------------------------------------------------------

module Numeric.AD 
    ( AD(..)
    , Mode(..)

    -- * Derivatives
    -- ** Forward AD
    , diffUU
    , diffUF

    , diff2UU
    , diff2UF

    -- ** Reverse AD
    , diffFU
    , diff2FU

    -- ** Forward Tower 
    , diffsUU
    , diffsUF

    , diffs0UU
    , diffs0UF

    , taylor
    , taylor0

    -- * Common access patterns
    , diff
    , diff2
    , diffs
    , diffs0

    -- * One-pass reverse mode gradient
    , grad, grad2
    , jacobian, jacobian2
    ) where

import Numeric.AD.Classes  (Mode(..))
import Numeric.AD.Internal (AD(..))
import Numeric.AD.Forward  (diff, diffUU, diff2, diff2UU, diffUF, diff2UF)
import Numeric.AD.Tower    (diffsUU, diffs0UU , diffsUF, diffs0UF , diffs, diffs0, taylor, taylor0) 
import Numeric.AD.Reverse  (diffFU, diff2FU, grad, grad2, jacobian, jacobian2)
