module Numeric.AD.Internal.Reverse.Double
       where


import Numeric.AD.Internal.Combinators
import Numeric.AD.Internal.Identity
import Numeric.AD.Jacobian
import Numeric.AD.Mode

data ReverseDouble = ReverseDouble { primal, tangent :: {-# UNPACK #-} !Double }
  deriving (Read, Show)

           
