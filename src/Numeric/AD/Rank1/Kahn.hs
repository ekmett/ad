{-# LANGUAGE CPP #-}

#define MODULE \
module Numeric.AD.Rank1.Kahn

#define AD_EXPORT Kahn

#define IMPORTS \
import Numeric.AD.Internal.Kahn

#define UNBINDWITH unbindWith
#define JACOBIAN jacobian
#define GRAD grad

#define AD_TYPE (Kahn a)
#define SCALAR_TYPE a
#define BASE0_1(x) x =>
#define BASE1_1(x,y) (x,y)
#define BASE2_1(x,y,z) (x,y,z)
#include "rank1_kahn.h"
