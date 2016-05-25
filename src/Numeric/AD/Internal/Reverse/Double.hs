{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
module Numeric.AD.Internal.Reverse.Double
       where
import Data.Reflection
import Data.Typeable
-- import System.IO.Unsafe (unsafePerformIO)

-- import Numeric.AD.Internal.Combinators
import Numeric.AD.Internal.Identity
import qualified Numeric.AD.Internal.Reverse as R (Tape)
import Numeric.AD.Jacobian
import Numeric.AD.Mode


-- newtype ReverseDouble s = ReverseDouble { unReverseDouble :: forall s. Reverse s Double }

#ifndef HLINT
data ReverseDouble s where
  ZeroR :: ReverseDouble s
  LiftR :: Double -> ReverseDouble s
  ReverseR :: {-# UNPACK #-} !Int -> Double -> ReverseDouble s
  deriving (Show, Typeable)
#endif


instance Reifies s R.Tape => Mode (ReverseDouble s) where
  type Scalar (ReverseDouble s) = Double
  
  isKnownZero ZeroR = True
  isKnownZero _ = False

  isKnownConstant ReverseR{} = False
  isKnownConstant _ = True
  
  auto = LiftR
  zero = ZeroR

  a *^ b = lift1 (a *) (\_ -> auto a) b
  a ^* b = lift1 (* b) (\_ -> auto b) a
  a ^/ b = lift1 (/ b) (\_ -> auto (recip b)) a

primal :: ReverseDouble s -> Double
primal ZeroR = 0
primal (LiftR a) = a
primal (ReverseR _ a) = a



instance Reifies s R.Tape => Jacobian (ReverseDouble s) where
  type D (ReverseDouble s) = Id Double

  -- unary f _         (Zero)   = Lift (f 0)
  -- unary f _         (Lift a) = Lift (f a)
  -- unary f (Id dadi) (Reverse i b) = unarily f dadi i b


-- | monomorphic `unarily` and `binarily`. 

-- unarily :: forall s. Reifies s R.Tape => (Double -> Double) -> Double -> Int -> Double -> ReverseDouble s
-- unarily f di i b = Reverse (unsafePerformIO (R.modifyTape (Proxy :: Proxy s) (R.un i di))) $! f b
-- {-# INLINE unarily #-}

-- binarily :: forall s. Reifies s R.Tape => (Double -> Double -> Double) -> Double -> Double -> Int -> Double -> Int -> Double -> ReverseDouble s
-- binarily f di dj i b j c = Reverse (unsafePerformIO (R.modifyTape (Proxy :: Proxy s) (R.bin i j di dj))) $! f b c
-- {-# INLINE binarily #-}



instance Num (ReverseDouble s) where
