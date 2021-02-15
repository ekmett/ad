{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
-- Copyright   :  (c) Edward Kmett 2010-2021
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

module Numeric.AD.Internal.Kahn.Double
  ( KahnDouble(..)
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
  , unbindWithUArray
  , unbindWithArray
  , unbindMapWithDefault
  , primal
  , var
  , varId
  ) where

import Control.Monad.ST
import Control.Monad hiding (mapM)
import Control.Monad.Trans.State
import Data.List (foldl')
import Data.Array.ST
import Data.Array.IArray
import qualified Data.Array as A
import Data.Array.Unboxed (UArray)
import Data.IntMap (IntMap, fromListWith, findWithDefault)
import Data.Graph (Vertex, transposeG, Graph)
import Data.Number.Erf
import Data.Reify (reifyGraph, MuRef(..))
import qualified Data.Reify.Graph as Reified
import System.IO.Unsafe (unsafePerformIO)
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Exts as Exts
import Numeric.AD.Internal.Combinators
import Numeric.AD.Internal.Identity
import Numeric.AD.Jacobian
import Numeric.AD.Mode

-- | A @Tape@ records the information needed back propagate from the output to each input during reverse 'Mode' AD.
data Tape t
  = Zero
  | Lift {-# UNPACK #-} !Double
  | Var {-# UNPACK #-} !Double {-# UNPACK #-} !Int
  | Binary {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double t t
  | Unary {-# UNPACK #-} !Double {-# UNPACK #-} !Double t
  deriving (Show, Data, Typeable)

-- | @Kahn@ is a 'Mode' using reverse-mode automatic differentiation that provides fast 'diffFU', 'diff2FU', 'grad', 'grad2' and a fast 'jacobian' when you have a significantly smaller number of outputs than inputs.
newtype KahnDouble = Kahn (Tape KahnDouble) deriving (Show, Typeable)

instance MuRef KahnDouble where
  type DeRef KahnDouble = Tape

  mapDeRef _ (Kahn Zero) = pure Zero
  mapDeRef _ (Kahn (Lift a)) = pure (Lift a)
  mapDeRef _ (Kahn (Var a v)) = pure (Var a v)
  mapDeRef f (Kahn (Binary a dadb dadc b c)) = Binary a dadb dadc <$> f b <*> f c
  mapDeRef f (Kahn (Unary a dadb b)) = Unary a dadb <$> f b

instance Mode KahnDouble where
  type Scalar KahnDouble = Double

  isKnownZero (Kahn Zero) = True
  isKnownZero (Kahn (Lift 0)) = True
  isKnownZero _    = False

  asKnownConstant (Kahn Zero) = Just 0
  asKnownConstant (Kahn (Lift n)) = Just n
  asKnownConstant _ = Nothing

  isKnownConstant (Kahn Zero) = True
  isKnownConstant (Kahn (Lift _)) = True
  isKnownConstant _ = False

  auto a = Kahn (Lift a)
  zero   = Kahn Zero

  a *^ b = lift1 (a *) (\_ -> auto a) b
  a ^* b = lift1 (* b) (\_ -> auto b) a
  a ^/ b = lift1 (/ b) (\_ -> auto (recip b)) a

(<+>) :: KahnDouble -> KahnDouble -> KahnDouble
(<+>)  = binary (+) 1 1

primal :: KahnDouble -> Double
primal (Kahn Zero) = 0
primal (Kahn (Lift a)) = a
primal (Kahn (Var a _)) = a
primal (Kahn (Binary a _ _ _ _)) = a
primal (Kahn (Unary a _ _)) = a

instance Jacobian KahnDouble where
  type D KahnDouble = Id Double

  unary f _         (Kahn Zero)     = Kahn (Lift (f 0))
  unary f _         (Kahn (Lift a)) = Kahn (Lift (f a))
  unary f (Id dadb) b               = Kahn (Unary (f (primal b)) dadb b)

  lift1 f df b = unary f (df (Id pb)) b where
    pb = primal b

  lift1_ f df b = unary (const a) (df (Id a) (Id pb)) b where
    pb = primal b
    a = f pb

  binary f _         _         (Kahn Zero)     (Kahn Zero)        = Kahn (Lift (f 0 0))
  binary f _         _         (Kahn Zero)     (Kahn (Lift c))    = Kahn (Lift (f 0 c))
  binary f _         _         (Kahn (Lift b)) (Kahn Zero)        = Kahn (Lift (f b 0))
  binary f _         _         (Kahn (Lift b)) (Kahn (Lift c))    = Kahn (Lift (f b c))
  binary f _         (Id dadc) (Kahn Zero)     c                  = Kahn (Unary (f 0 (primal c)) dadc c)
  binary f _         (Id dadc) (Kahn (Lift b)) c                  = Kahn (Unary (f b (primal c)) dadc c)
  binary f (Id dadb) _         b                  (Kahn Zero)     = Kahn (Unary (f (primal b) 0) dadb b)
  binary f (Id dadb) _         b                  (Kahn (Lift c)) = Kahn (Unary (f (primal b) c) dadb b)
  binary f (Id dadb) (Id dadc) b                  c               = Kahn (Binary (f (primal b) (primal c)) dadb dadc b c)

  lift2 f df b c = binary f dadb dadc b c where
    (dadb, dadc) = df (Id (primal b)) (Id (primal c))

  lift2_ f df b c = binary (\_ _ -> a) dadb dadc b c where
    pb = primal b
    pc = primal c
    a = f pb pc
    (dadb, dadc) = df (Id a) (Id pb) (Id pc)


mul :: KahnDouble -> KahnDouble -> KahnDouble
mul = lift2 (*) (\x y -> (y, x))

#define HEAD KahnDouble
#define BODY1(x)
#define BODY2(x,y)
#define NO_Bounded
#include <instances.h>

derivative :: KahnDouble -> Double
derivative = sum . map snd . partials
{-# INLINE derivative #-}

derivative' :: KahnDouble -> (Double, Double)
derivative' r = (primal r, derivative r)
{-# INLINE derivative' #-}

-- | back propagate sensitivities along a tape.
backPropagate :: (Vertex -> (Tape Int, Int, [Int])) -> STUArray s Int Double -> Vertex -> ST s ()
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
partials :: KahnDouble -> [(Int, Double)]

partials tape = [ let v = sensitivities ! ix in seq v (ident, v) | (ix, Var _ ident) <- xs ] where
  Reified.Graph xs start = unsafePerformIO $ reifyGraph tape
  g = array xsBounds [ (i, successors t) | (i, t) <- xs ]
  vertexMap = A.array xsBounds xs
  vmap i = (vertexMap ! i, i, [])
  xsBounds = sbounds xs

  sensitivities = runSTUArray $ do
    ss <- newArray xsBounds 0
    writeArray ss start 1
    forM_ (topSortAcyclic g) $
      backPropagate vmap ss
    return ss

  sbounds ((a,_):as) = foldl' (\(lo,hi) (b,_) -> let lo' = min lo b; hi' = max hi b in lo' `seq` hi' `seq` (lo', hi')) (a,a) as
  sbounds _ = undefined -- the graph can't be empty, it contains the output node!

  successors :: Tape Int -> [Int]
  successors (Unary _ _ b) = [b]
  successors (Binary _ _ _ b c) = if b == c then [b] else [b,c]
  successors _ = []

-- | Return an 'Array' of 'partials' given bounds for the variable IDs.
partialArray :: (Int, Int) -> KahnDouble -> UArray Int Double
partialArray vbounds tape = accumArray (+) 0 vbounds (partials tape)
{-# INLINE partialArray #-}

-- | Return an 'IntMap' of sparse partials
partialMap :: KahnDouble -> IntMap Double
partialMap = fromListWith (+) . partials
{-# INLINE partialMap #-}


-- strict list of doubles
data Doubles = Nil | Cons !Double !Doubles

instance IsList Doubles where
  type Item Doubles = Double
  fromList (x:xs) = Cons x (fromList xs)
  fromList [] = Nil
  toList Nil = []
  toList (Cons x xs) = x : toList xs

class Grad i o o' | i -> o o', o -> i o', o' -> i o where
  pack :: i -> [KahnDouble] -> KahnDouble
  unpack :: (Doubles -> Doubles) -> o
  unpack' :: (Doubles -> (Double, Doubles)) -> o'

instance Grad KahnDouble Doubles (Double, Doubles) where
  pack i _ = i
  unpack f = f Nil
  unpack' f = f Nil

instance Grad i o o' => Grad (KahnDouble -> i) (Double -> o) (Double -> o') where
  pack f (a:as) = pack (f a) as
  pack _ [] = error "Grad.pack: logic error"
  unpack f a = unpack (f . Cons a)
  unpack' f a = unpack' (f . Cons a)

vgrad :: Grad i o o' => i -> o
vgrad i = unpack (unsafeGrad (pack i)) where
  unsafeGrad f as = unbinds vs (partialArray bds $ f vs) where
    (vs,bds) = binds as

vgrad' :: Grad i o o' => i -> o'
vgrad' i = unpack' (unsafeGrad' (pack i)) where
  unsafeGrad' f as = (primal r, unbinds vs (partialArray bds r)) where
    r = f vs
    (vs,bds) = binds as

var :: Double -> Int -> KahnDouble
var a v = Kahn (Var a v)

varId :: KahnDouble -> Int
varId (Kahn (Var _ v)) = v
varId _ = error "varId: not a Var"

bind :: Traversable f => f Double -> (f KahnDouble, (Int,Int))
bind xs = (r,(0,hi)) where
  (r,hi) = runState (mapM freshVar xs) 0
  freshVar a = state $ \s -> let s' = s + 1 in s' `seq` (var a s, s')

binds :: Doubles -> ([KahnDouble], (Int,Int))
binds = bind . Exts.toList

unbind :: Functor f => f KahnDouble -> UArray Int Double -> f Double
unbind xs ys = fmap (\v -> ys ! varId v) xs

unbinds :: Foldable f => f KahnDouble -> UArray Int Double -> Doubles
unbinds xs ys = foldr (\v r -> Cons (ys ! varId v) r) Nil xs

unbindWithUArray :: (Functor f, IArray UArray b) => (Double -> b -> c) -> f KahnDouble -> UArray Int b -> f c
unbindWithUArray f xs ys = fmap (\v -> f (primal v) (ys ! varId v)) xs

unbindWithArray :: Functor f => (Double -> b -> c) -> f KahnDouble -> Array Int b -> f c
unbindWithArray f xs ys = fmap (\v -> f (primal v) (ys ! varId v)) xs

unbindMap :: Functor f => f KahnDouble -> IntMap Double -> f Double
unbindMap xs ys = fmap (\v -> findWithDefault 0 (varId v) ys) xs

unbindMapWithDefault :: Functor f => b -> (Double -> b -> c) -> f KahnDouble -> IntMap b -> f c
unbindMapWithDefault z f xs ys = fmap (\v -> f (primal v) $ findWithDefault z (varId v) ys) xs
