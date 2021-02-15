{-# LANGUAGE CPP #-}

#define MODULE \
module Numeric.AD.Internal.Kahn.Double
#define IMPORTS
#define AD_EXPORT KahnDouble(..)
#define AD_TYPE KahnDouble
#define SCALAR_TYPE Double
#include <internal_kahn.h>
