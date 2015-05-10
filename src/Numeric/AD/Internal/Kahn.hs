{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2010-2015
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- This module provides reverse-mode Automatic Differentiation implementation using
-- linear time topological sorting after the fact.
--
-- For this form of reverse-mode AD we use 'System.Mem.StableName.StableName' to recover
-- sharing information from the tape to avoid combinatorial explosion, and thus
-- run asymptotically faster than it could without such sharing information, but the use
-- of side-effects contained herein is benign.
--
-----------------------------------------------------------------------------

module Numeric.AD.Internal.Kahn
  ( Kahn(..)
  , Tape(..)
  , partials
  , partialArray
  , partialMap
  , derivative
  , derivative'
  , vgrad, vgrad'
  , Grad(..)
  , bind
  , unbind
  , unbindMap
  , unbindWith
  , unbindMapWithDefault
  , primal
  , var
  , varId
  ) where

import Prelude hiding (mapM)
import Control.Applicative (Applicative(..),(<$>))
import Control.Monad.ST
import Control.Monad hiding (mapM)
import Control.Monad.Trans.State
import Data.List (foldl')
import Data.Array.ST
import Data.Array
import Data.IntMap (IntMap, fromListWith, findWithDefault)
import Data.Graph (Vertex, transposeG, Graph)
import Data.Number.Erf
import Data.Reify (reifyGraph, MuRef(..))
import qualified Data.Reify.Graph as Reified
import System.IO.Unsafe (unsafePerformIO)
import Data.Data (Data)
import Data.Traversable (Traversable, mapM)
import Data.Typeable (Typeable)
import Numeric.AD.Internal.Combinators
import Numeric.AD.Internal.Identity
import Numeric.AD.Jacobian
import Numeric.AD.Mode

-- | A @Tape@ records the information needed back propagate from the output to each input during reverse 'Mode' AD.
data Tape a t
  = Zero
  | Lift !a
  | Var !a {-# UNPACK #-} !Int
  | Binary !a a a t t
  | Unary !a a t
  deriving (Show, Data, Typeable)

-- | @Kahn@ is a 'Mode' using reverse-mode automatic differentiation that provides fast 'diffFU', 'diff2FU', 'grad', 'grad2' and a fast 'jacobian' when you have a significantly smaller number of outputs than inputs.
newtype Kahn a = Kahn (Tape a (Kahn a)) deriving (Show, Typeable)

instance MuRef (Kahn a) where
  type DeRef (Kahn a) = Tape a

  mapDeRef _ (Kahn Zero) = pure Zero
  mapDeRef _ (Kahn (Lift a)) = pure (Lift a)
  mapDeRef _ (Kahn (Var a v)) = pure (Var a v)
  mapDeRef f (Kahn (Binary a dadb dadc b c)) = Binary a dadb dadc <$> f b <*> f c
  mapDeRef f (Kahn (Unary a dadb b)) = Unary a dadb <$> f b

instance Num a => Mode (Kahn a) where
  type Scalar (Kahn a) = a

  isKnownZero (Kahn Zero) = True
  isKnownZero _    = False

  isKnownConstant (Kahn Zero) = True
  isKnownConstant (Kahn (Lift _)) = True
  isKnownConstant _ = False

  auto a = Kahn (Lift a)
  zero   = Kahn Zero
  a *^ b = lift1 (a *) (\_ -> auto a) b
  a ^* b = lift1 (* b) (\_ -> auto b) a
  a ^/ b = lift1 (/ b) (\_ -> auto (recip b)) a

(<+>) :: Num a => Kahn a -> Kahn a -> Kahn a
(<+>)  = binary (+) 1 1

(<**>) :: Floating a => Kahn a -> Kahn a -> Kahn a
Kahn Zero <**> y             = auto (0 ** primal y)
_         <**> Kahn Zero     = auto 1
x         <**> Kahn (Lift y) = lift1 (**y) (\z -> y *^ z ** Id (y-1)) x
x         <**> y             = lift2_ (**) (\z xi yi -> (yi * z / xi, z * xi)) x y

primal :: Num a => Kahn a -> a
primal (Kahn Zero) = 0
primal (Kahn (Lift a)) = a
primal (Kahn (Var a _)) = a
primal (Kahn (Binary a _ _ _ _)) = a
primal (Kahn (Unary a _ _)) = a

instance Num a => Jacobian (Kahn a) where
  type D (Kahn a) = Id a

  unary f _         (Kahn Zero)     = Kahn (Lift (f 0))
  unary f _         (Kahn (Lift a)) = Kahn (Lift (f a))
  unary f (Id dadb) b                  = Kahn (Unary (f (primal b)) dadb b)

  lift1 f df b = unary f (df (Id pb)) b where
    pb = primal b

  lift1_ f df b = unary (const a) (df (Id a) (Id pb)) b where
    pb = primal b
    a = f pb

  binary f _         _         (Kahn Zero)     (Kahn Zero)     = Kahn (Lift (f 0 0))
  binary f _         _         (Kahn Zero)     (Kahn (Lift c)) = Kahn (Lift (f 0 c))
  binary f _         _         (Kahn (Lift b)) (Kahn Zero)     = Kahn (Lift (f b 0))
  binary f _         _         (Kahn (Lift b)) (Kahn (Lift c)) = Kahn (Lift (f b c))
  binary f _         (Id dadc) (Kahn Zero)     c                  = Kahn (Unary (f 0 (primal c)) dadc c)
  binary f _         (Id dadc) (Kahn (Lift b)) c                  = Kahn (Unary (f b (primal c)) dadc c)
  binary f (Id dadb) _         b                  (Kahn Zero)     = Kahn (Unary (f (primal b) 0) dadb b)
  binary f (Id dadb) _         b                  (Kahn (Lift c)) = Kahn (Unary (f (primal b) c) dadb b)
  binary f (Id dadb) (Id dadc) b                  c                  = Kahn (Binary (f (primal b) (primal c)) dadb dadc b c)

  lift2 f df b c = binary f dadb dadc b c where
    (dadb, dadc) = df (Id (primal b)) (Id (primal c))

  lift2_ f df b c = binary (\_ _ -> a) dadb dadc b c where
    pb = primal b
    pc = primal c
    a = f pb pc
    (dadb, dadc) = df (Id a) (Id pb) (Id pc)

#define HEAD Kahn a
#include <instances.h>

derivative :: Num a => Kahn a -> a
derivative = sum . map snd . partials
{-# INLINE derivative #-}

derivative' :: Num a => Kahn a -> (a, a)
derivative' r = (primal r, derivative r)
{-# INLINE derivative' #-}

-- | back propagate sensitivities along a tape.
backPropagate :: Num a => (Vertex -> (Tape a Int, Int, [Int])) -> STArray s Int a -> Vertex -> ST s ()
backPropagate vmap ss v = case node of
  Unary _ g b -> do
    da <- readArray ss i
    db <- readArray ss b
    writeArray ss b (db + g*da)
  Binary _ gb gc b c -> do
    da <- readArray ss i
    db <- readArray ss b
    writeArray ss b (db + gb*da)
    dc <- readArray ss c
    writeArray ss c (dc + gc*da)
  _ -> return ()
  where
    (node, i, _) = vmap v
    -- this isn't _quite_ right, as it should allow negative zeros to multiply through

topSortAcyclic :: Graph -> [Vertex]
topSortAcyclic g = reverse $ runST $ do
  del <- newArray (bounds g) False :: ST s (STUArray s Int Bool)
  let tg = transposeG g
      starters = [ n | (n, []) <- assocs tg ]
      loop [] rs = return rs
      loop (n:ns) rs = do
        writeArray del n True
        let add [] = return ns
            add (m:ms) = do
              b <- ok (tg!m)
              ms' <- add ms
              return $ if b then m : ms' else ms'
            ok [] = return True
            ok (x:xs) = do b <- readArray del x; if b then ok xs else return False
        ns' <- add (g!n)
        loop ns' (n : rs)
  loop starters []

-- | This returns a list of contributions to the partials.
-- The variable ids returned in the list are likely /not/ unique!
{-# SPECIALIZE partials :: Kahn Double -> [(Int, Double)] #-}
partials :: forall a. Num a => Kahn a -> [(Int, a)]
partials tape = [ let v = sensitivities ! ix in seq v (ident, v) | (ix, Var _ ident) <- xs ] where
  Reified.Graph xs start = unsafePerformIO $ reifyGraph tape
  g = array xsBounds [ (i, successors t) | (i, t) <- xs ]
  vertexMap = array xsBounds xs
  vmap i = (vertexMap ! i, i, [])
  xsBounds = sbounds xs

  sensitivities = runSTArray $ do
    ss <- newArray xsBounds 0
    writeArray ss start 1
    forM_ (topSortAcyclic g) $
      backPropagate vmap ss
    return ss

  sbounds ((a,_):as) = foldl' (\(lo,hi) (b,_) -> let lo' = min lo b; hi' = max hi b in lo' `seq` hi' `seq` (lo', hi')) (a,a) as
  sbounds _ = undefined -- the graph can't be empty, it contains the output node!

  successors :: Tape a t -> [t]
  successors (Unary _ _ b) = [b]
  successors (Binary _ _ _ b c) = [b,c]
  successors _ = []

-- | Return an 'Array' of 'partials' given bounds for the variable IDs.
partialArray :: Num a => (Int, Int) -> Kahn a -> Array Int a
partialArray vbounds tape = accumArray (+) 0 vbounds (partials tape)
{-# INLINE partialArray #-}

-- | Return an 'IntMap' of sparse partials
partialMap :: Num a => Kahn a -> IntMap a
partialMap = fromListWith (+) . partials
{-# INLINE partialMap #-}

class Num a => Grad i o o' a | i -> a o o', o -> a i o', o' -> a i o where
  pack :: i -> [Kahn a] -> Kahn a
  unpack :: ([a] -> [a]) -> o
  unpack' :: ([a] -> (a, [a])) -> o'

instance Num a => Grad (Kahn a) [a] (a, [a]) a where
  pack i _ = i
  unpack f = f []
  unpack' f = f []

instance Grad i o o' a => Grad (Kahn a -> i) (a -> o) (a -> o') a where
  pack f (a:as) = pack (f a) as
  pack _ [] = error "Grad.pack: logic error"
  unpack f a = unpack (f . (a:))
  unpack' f a = unpack' (f . (a:))

vgrad :: Grad i o o' a => i -> o
vgrad i = unpack (unsafeGrad (pack i)) where
  unsafeGrad f as = unbind vs (partialArray bds $ f vs) where
    (vs,bds) = bind as

vgrad' :: Grad i o o' a => i -> o'
vgrad' i = unpack' (unsafeGrad' (pack i)) where
  unsafeGrad' f as = (primal r, unbind vs (partialArray bds r)) where
    r = f vs
    (vs,bds) = bind as

var :: a -> Int -> Kahn a
var a v = Kahn (Var a v)

varId :: Kahn a -> Int
varId (Kahn (Var _ v)) = v
varId _ = error "varId: not a Var"

bind :: Traversable f => f a -> (f (Kahn a), (Int,Int))
bind xs = (r,(0,hi)) where
  (r,hi) = runState (mapM freshVar xs) 0
  freshVar a = state $ \s -> let s' = s + 1 in s' `seq` (var a s, s')

unbind :: Functor f => f (Kahn a) -> Array Int a -> f a
unbind xs ys = fmap (\v -> ys ! varId v) xs

unbindWith :: (Functor f, Num a) => (a -> b -> c) -> f (Kahn a) -> Array Int b -> f c
unbindWith f xs ys = fmap (\v -> f (primal v) (ys ! varId v)) xs

unbindMap :: (Functor f, Num a) => f (Kahn a) -> IntMap a -> f a
unbindMap xs ys = fmap (\v -> findWithDefault 0 (varId v) ys) xs

unbindMapWithDefault :: (Functor f, Num a) => b -> (a -> b -> c) -> f (Kahn a) -> IntMap b -> f c
unbindMapWithDefault z f xs ys = fmap (\v -> f (primal v) $ findWithDefault z (varId v) ys) xs
