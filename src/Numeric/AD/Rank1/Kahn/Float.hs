{-# LANGUAGE CPP #-}

#define MODULE \
module Numeric.AD.Rank1.Kahn.Float

#define AD_EXPORT KahnFloat

#define IMPORTS \
import Numeric.AD.Internal.Kahn (Kahn); \
import qualified Numeric.AD.Rank1.Kahn as Kahn; \
import Numeric.AD.Internal.Kahn.Float

#define UNBINDWITH unbindWithUArray
#define GRAD Kahn.grad
#define JACOBIAN Kahn.jacobian

#define AD_TYPE KahnFloat
#define SCALAR_TYPE Float
#define BASE0_1(x)
#define BASE1_1(x,y) x
#define BASE2_1(x,y,z) (x,y)
#include "rank1_kahn.h"
