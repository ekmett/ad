{-# LANGUAGE CPP #-}

#define MODULE \
module Numeric.AD.Internal.Kahn.Float
#define IMPORTS
#define AD_EXPORT KahnFloat(..)
#define AD_TYPE KahnFloat
#define SCALAR_TYPE Float
#include <internal_kahn.h>
