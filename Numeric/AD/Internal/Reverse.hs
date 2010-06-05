{-# LANGUAGE Rank2Types, TypeFamilies, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TemplateHaskell, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Internal.Reverse
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Reverse-Mode Automatic Differentiation implementation details
--
-- For reverse mode AD we use 'System.Mem.StableName.StableName' to recover sharing information from
-- the tape to avoid combinatorial explosion, and thus run asymptotically faster
-- than it could without such sharing information, but the use of side-effects
-- contained herein is benign.
--
-----------------------------------------------------------------------------

module Numeric.AD.Internal.Reverse
    ( Reverse(..)
    , Tape(..)
    , partials
    , partialArray
    , partialMap
    , derivative
    , derivative2
    , Var(..)
    ) where

import Prelude hiding (mapM)
import Control.Applicative (Applicative(..),(<$>))
import Control.Monad.ST
import Control.Monad (forM_)
import Data.List (foldl')
import Data.Array.ST
import Data.Array
import Data.IntMap (IntMap, fromListWith, findWithDefault)
import Data.Graph (graphFromEdges', topSort, Vertex)
import Data.Reify (reifyGraph, MuRef(..))
import qualified Data.Reify.Graph as Reified
import Data.Traversable (Traversable, mapM)
import System.IO.Unsafe (unsafePerformIO)
import Language.Haskell.TH

import Numeric.AD.Classes
import Numeric.AD.Internal

-- | A @Tape@ records the information needed back propagate from the output to each input during 'Reverse' 'Mode' AD.
data Tape a t
    = Lift a
    | Var a {-# UNPACK #-} !Int
    | Binary a a a t t
    | Unary a a t
    deriving (Show)

-- | @Reverse@ is a 'Mode' using reverse-mode automatic differentiation that provides fast 'diffFU', 'diff2FU', 'grad', 'grad2' and a fast 'jacobian' when you have a significantly smaller number of outputs than inputs.
newtype Reverse a = Reverse (Tape a (Reverse a)) deriving (Show)

instance MuRef (Reverse a) where
    type DeRef (Reverse a) = Tape a

    mapDeRef _ (Reverse (Lift a)) = pure (Lift a)
    mapDeRef _ (Reverse (Var a v)) = pure (Var a v)
    mapDeRef f (Reverse (Binary a dadb dadc b c)) = Binary a dadb dadc <$> f b <*> f c
    mapDeRef f (Reverse (Unary a dadb b)) = Unary a dadb <$> f b

instance Lifted Reverse => Mode Reverse where
    lift a = Reverse (Lift a)
    (<+>)  = binary (+) one one
    a *^ b = lift1 (a *) (\_ -> lift a) b
    a ^* b = lift1 (* b) (\_ -> lift b) a
    a ^/ b = lift1 (/ b) (\_ -> lift (recip b)) a

instance Primal Reverse where
    primal (Reverse (Lift a)) = a
    primal (Reverse (Var a _)) = a
    primal (Reverse (Binary a _ _ _ _)) = a
    primal (Reverse (Unary a _ _)) = a

instance Lifted Reverse => Jacobian Reverse where
    type D Reverse = Id

    unary f _         (Reverse (Lift a)) = Reverse (Lift (f a))
    unary f (Id dadb) b                  = Reverse (Unary (f (primal b)) dadb b)

    lift1 f df b = unary f (df (Id pb)) b
        where pb = primal b

    lift1_ f df b = unary (const a) (df (Id a) (Id pb)) b
        where pb = primal b
              a = f pb

    binary f _         _         (Reverse (Lift b)) (Reverse (Lift c)) = Reverse (Lift (f b c))
    binary f _         (Id dadc) (Reverse (Lift b)) c                  = Reverse (Unary (f b (primal c)) dadc c)
    binary f (Id dadb) _         b                  (Reverse (Lift c)) = Reverse (Unary (f (primal b) c) dadb b)
    binary f (Id dadb) (Id dadc) b                  c                  = Reverse (Binary (f (primal b) (primal c)) dadb dadc b c)

    lift2 f df b c = binary f dadb dadc b c
        where (dadb, dadc) = df (Id (primal b)) (Id (primal c))

    lift2_ f df b c = binary (\_ _ -> a) dadb dadc b c
        where
            pb = primal b
            pc = primal c
            a = f pb pc
            (dadb, dadc) = df (Id a) (Id pb) (Id pc)

deriveLifted (conT ''Reverse)
-- deriveNumeric  ''Reverse

derivative :: Num a => AD Reverse a -> a
derivative = sum . map snd . partials
{-# INLINE derivative #-}

derivative2 :: Num a => AD Reverse a -> (a, a)
derivative2 r = (primal r, derivative r)
{-# INLINE derivative2 #-}

-- | back propagate sensitivities along a tape.
backPropagate :: Num a => (Vertex -> (Tape a Int, Int, [Int])) -> STArray s Int a -> Vertex -> ST s ()
backPropagate vmap ss v = do
        case node of
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

-- | This returns a list of contributions to the partials.
-- The variable ids returned in the list are likely /not/ unique!
partials :: Num a => AD Reverse a -> [(Int, a)]
partials (AD tape) = [ (ident, sensitivities ! ix) | (ix, Var _ ident) <- xs ]
    where
        Reified.Graph xs start = unsafePerformIO $ reifyGraph tape
        (g, vmap) = graphFromEdges' (edgeSet <$> filter nonConst xs)
        sensitivities = runSTArray $ do
            ss <- newArray (sbounds xs) 0
            writeArray ss start 1
            forM_ (topSort g) $
                backPropagate vmap ss
            return ss
        sbounds ((a,_):as) = foldl' (\(lo,hi) (b,_) -> (min lo b, max hi b)) (a,a) as
        sbounds _ = undefined -- the graph can't be empty, it contains the output node!
        edgeSet (i, t) = (t, i, successors t)
        nonConst (_, Lift{}) = False
        nonConst _ = True
        successors (Unary _ _ b) = [b]
        successors (Binary _ _ _ b c) = [b,c]
        successors _ = []

-- | Return an 'Array' of 'partials' given bounds for the variable IDs.
partialArray :: Num a => (Int, Int) -> AD Reverse a -> Array Int a
partialArray vbounds tape = accumArray (+) 0 vbounds (partials tape)
{-# INLINE partialArray #-}

-- | Return an 'IntMap' of sparse partials
partialMap :: Num a => AD Reverse a -> IntMap a
partialMap = fromListWith (+) . partials
{-# INLINE partialMap #-}

-- A simple fresh variable supply monad
newtype S a = S { runS :: Int -> (a,Int) }
instance Monad S where
    return a = S (\s -> (a,s))
    S g >>= f = S (\s -> let (a,s') = g s in runS (f a) s')

-- | Used to mark variables for inspection during the reverse pass
class Var t a | t -> a where
    var     :: a -> Int -> t
    fromVar :: t -> Int

    bind :: Traversable f => f a -> (f t, (Int,Int))
    unbind :: Functor f => f t -> Array Int a -> f a
    unbindMap :: (Functor f, Num a) => f t -> IntMap a -> f a

    -- TODO: tweak bounds
    bind xs = (r,(0,hi))
        where
        (r,hi) = runS (mapM freshVar xs) 0
        freshVar a = S (\s -> let s' = s + 1 in s' `seq` (var a s, s'))
    unbind xs ys = fmap (\v -> ys ! fromVar v) xs
    unbindMap xs ys = fmap (\v -> findWithDefault 0 (fromVar v) ys) xs

instance Var (Reverse a) a where
    var a v = Reverse (Var a v)
    fromVar (Reverse (Var _ v)) = v
    fromVar _ = error "fromVar: not a Var"

instance Var (AD Reverse a) a where
    var a v = AD (var a v)
    fromVar (AD v) = fromVar v
