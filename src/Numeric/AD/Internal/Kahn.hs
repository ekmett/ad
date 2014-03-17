{-# LANGUAGE Rank2Types, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, ScopedTypeVariables, TemplateHaskell, TypeFamilies, DeriveDataTypeable, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

-- {-# OPTIONS_HADDOCK hide, prune #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Internal.Kahn
-- Copyright   :  (c) Edward Kmett 2010
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
    ) where

import Prelude hiding (mapM)
import Control.Applicative (Applicative(..),(<$>))
import Control.Monad.ST
import Control.Monad (forM_)
import Data.List (foldl')
import Data.Array.ST
import Data.Array
import Data.IntMap (IntMap, fromListWith)
import Data.Graph (Vertex, transposeG, Graph)
import Data.Reify (reifyGraph, MuRef(..))
import qualified Data.Reify.Graph as Reified
import System.IO.Unsafe (unsafePerformIO)
import Language.Haskell.TH
import Data.Data (Data)
import Data.Typeable (Typeable)
import Numeric.AD.Internal.Classes
import Numeric.AD.Internal.Identity
import Numeric.AD.Internal.Var

-- | A @Tape@ records the information needed back propagate from the output to each input during reverse 'Mode' AD.
data Tape a t
    = Zero
    | Lift !a
    | Var !a {-# UNPACK #-} !Int
    | Binary !a a a t t
    | Unary !a a t
    deriving (Show, Data, Typeable)

-- | @Kahn@ is a 'Mode' using reverse-mode automatic differentiation that provides fast 'diffFU', 'diff2FU', 'grad', 'grad2' and a fast 'jacobian' when you have a significantly smaller number of outputs than inputs.
newtype Kahn a s = Kahn (Tape a (Kahn a s)) deriving (Show, Typeable)

type instance Scalar (Kahn a s) = a

-- deriving instance (Data (Tape a (Kahn a)) => Data (Kahn a)

instance MuRef (Kahn a s) where
    type DeRef (Kahn a s) = Tape a

    mapDeRef _ (Kahn Zero) = pure Zero
    mapDeRef _ (Kahn (Lift a)) = pure (Lift a)
    mapDeRef _ (Kahn (Var a v)) = pure (Var a v)
    mapDeRef f (Kahn (Binary a dadb dadc b c)) = Binary a dadb dadc <$> f b <*> f c
    mapDeRef f (Kahn (Unary a dadb b)) = Unary a dadb <$> f b

instance Num a => Mode (Kahn a s) where
    isKnownZero (Kahn Zero) = True
    isKnownZero _    = False

    isKnownConstant (Kahn Zero) = True
    isKnownConstant (Kahn (Lift _)) = True
    isKnownConstant _ = False

    auto a = Kahn (Lift a)
    zero   = Kahn Zero
    (<+>)  = binary (+) one one
    a *^ b = lift1 (a *) (\_ -> auto a) b
    a ^* b = lift1 (* b) (\_ -> auto b) a
    a ^/ b = lift1 (/ b) (\_ -> auto (recip b)) a

    Kahn Zero <**> y                = auto (0 ** primal y)
    _            <**> Kahn Zero     = auto 1
    x            <**> Kahn (Lift y) = lift1 (**y) (\z -> y *^ z ** Id (y-1)) x
    x            <**> y                = lift2_ (**) (\z xi yi -> (yi * z / xi, z * xi)) x y

instance Num a => Primal (Kahn a s) where
    primal (Kahn Zero) = 0
    primal (Kahn (Lift a)) = a
    primal (Kahn (Var a _)) = a
    primal (Kahn (Binary a _ _ _ _)) = a
    primal (Kahn (Unary a _ _)) = a

instance Num a => Jacobian (Kahn a s) where
    type D (Kahn a s) = Id a s

    unary f _         (Kahn Zero)     = Kahn (Lift (f 0))
    unary f _         (Kahn (Lift a)) = Kahn (Lift (f a))
    unary f (Id dadb) b                  = Kahn (Unary (f (primal b)) dadb b)

    lift1 f df b = unary f (df (Id pb)) b
        where pb = primal b

    lift1_ f df b = unary (const a) (df (Id a) (Id pb)) b
        where pb = primal b
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

    lift2 f df b c = binary f dadb dadc b c
        where (dadb, dadc) = df (Id (primal b)) (Id (primal c))

    lift2_ f df b c = binary (\_ _ -> a) dadb dadc b c
        where
            pb = primal b
            pc = primal c
            a = f pb pc
            (dadb, dadc) = df (Id a) (Id pb) (Id pc)

let s = VarT (mkName "s") in deriveNumeric id (ConT ''Kahn) s

derivative :: Num a => Kahn a s -> a
derivative = sum . map snd . partials
{-# INLINE derivative #-}

derivative' :: Num a => Kahn a s -> (a, a)
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
{-# SPECIALIZE partials :: Kahn Double s -> [(Int, Double)] #-}
partials :: forall s a . Num a => Kahn a s -> [(Int, a)]
partials tape = [ let v = sensitivities ! ix in seq v (ident, v) | (ix, Var _ ident) <- xs ]
    where
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
partialArray :: Num a => (Int, Int) -> Kahn a s -> Array Int a
partialArray vbounds tape = accumArray (+) 0 vbounds (partials tape)
{-# INLINE partialArray #-}

-- | Return an 'IntMap' of sparse partials
partialMap :: Num a => Kahn a s -> IntMap a
partialMap = fromListWith (+) . partials
{-# INLINE partialMap #-}

-- A simple fresh variable supply monad
newtype S a = S { runS :: Int -> (a,Int) }
instance Monad S where
    return a = S (\s -> (a,s))
    S g >>= f = S (\s -> let (a,s') = g s in runS (f a) s')

instance Num a => Var (Kahn a s) where
    var a v = Kahn (Var a v)
    varId (Kahn (Var _ v)) = v
    varId _ = error "varId: not a Var"

class Num a => Grad i o o' a | i -> a o o', o -> a i o', o' -> a i o where
    pack :: i -> [Kahn a ()] -> Kahn a ()
    unpack :: ([a] -> [a]) -> o
    unpack' :: ([a] -> (a, [a])) -> o'

instance Num a => Grad (Kahn a ()) [a] (a, [a]) a where
    pack i _ = i
    unpack f = f []
    unpack' f = f []

instance Grad i o o' a => Grad (Kahn a () -> i) (a -> o) (a -> o') a where
    pack f (a:as) = pack (f a) as
    pack _ [] = error "Grad.pack: logic error"
    unpack f a = unpack (f . (a:))
    unpack' f a = unpack' (f . (a:))

vgrad :: Grad i o o' a => i -> o
vgrad i = unpack (unsafeGrad (pack i))
    where
        unsafeGrad f as = unbind vs (partialArray bds $ f vs)
            where
                (vs,bds) = bind as

vgrad' :: Grad i o o' a => i -> o'
vgrad' i = unpack' (unsafeGrad' (pack i))
    where
        unsafeGrad' f as = (primal r, unbind vs (partialArray bds r))
            where
                r = f vs
                (vs,bds) = bind as

