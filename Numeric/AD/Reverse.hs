{-# LANGUAGE Rank2Types, TypeFamilies, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TemplateHaskell, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Reverse
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only 
--
-- Mixed-Mode Automatic Differentiation.
-- 
-- For reverse mode AD we use 'System.Mem.StableName.StableName' to recover sharing information from 
-- the tape to avoid combinatorial explosion, and thus run asymptotically faster
-- than it could without such sharing information, but the use of side-effects
-- contained herein is benign.
--
-----------------------------------------------------------------------------

module Numeric.AD.Reverse
    ( Mode(..)
    , AD
    -- * Derivatives
    , diffUU
    , diff2UU
    , diffFU
    , diff2FU
    -- * Inefficient access patterns
    , diffUF
    , diff2UF
    -- * Common access patterns
    , diff, diff2
    , grad, grad2
    , jacobian, jacobian2
    -- * Internals
    -- ** Partial Derivatives
    , partials
    , partialArray
    , partialMap
    , derivative
    , derivative2
    -- * Data types
    , Tape(..)
    , Reverse(..)
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

    mapDeRef f (Reverse (Lift a)) = pure (Lift a)
    mapDeRef f (Reverse (Var a v)) = pure (Var a v)
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

-- | The 'grad' function calculates the gradient of a non-scalar-to-scalar function with 'Reverse' AD in a single pass.
grad :: (Traversable f, Num a) => (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> f a
grad f as = unbind vs (partialArray bounds $ f vs)
    where (vs,bounds) = bind as
{-# INLINE grad #-}

-- | The 'grad2' function calculates the result and gradient of a non-scalar-to-scalar function with 'Reverse' AD in a single pass.
grad2 :: (Traversable f, Num a) => (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> (a, f a)
grad2 f as = (primal r, unbind vs $ partialArray bounds r)
    where (vs, bounds) = bind as
          r = f vs 
{-# INLINE grad2 #-}

-- | The 'jacobian' function calculates the jacobian of a non-scalar-to-non-scalar function with reverse AD lazily in @m@ passes for @m@ outputs.
jacobian :: (Traversable f, Functor g, Num a) => (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (f a)
jacobian f as = unbind vs . partialArray bounds <$> f vs where 
    (vs, bounds) = bind as
{-# INLINE jacobian #-}

-- | The 'jacobian2' function calcualtes both the result and the Jacobian of a nonscalar-to-nonscalar function, using @m@ invocations of reverse AD,
-- where @m@ is the output dimensionality. Applying @fmap snd@ to the result will recover the result of 'jacobian'
jacobian2 :: (Traversable f, Functor g, Num a) => (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (a, f a)
jacobian2 f as = row <$> f vs where 
    (vs, bounds) = bind as
    row a = (primal a, unbind vs (partialArray bounds a))
{-# INLINE jacobian2 #-}

derivative :: Num a => AD Reverse a -> a
derivative = sum . map snd . partials
{-# INLINE derivative #-}

derivative2 :: Num a => AD Reverse a -> (a, a)
derivative2 r = (primal r, derivative r)
{-# INLINE derivative2 #-}

diffUU :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> a
diffUU f a = derivative $ f (var a 0)
{-# INLINE diffUU #-}

diffUF :: (Functor f, Num a) => (forall s. Mode s => AD s a -> f (AD s a)) -> a -> f a
diffUF f a = derivative <$> f (var a 0)
{-# INLINE diffUF #-}

-- | The 'diff2UU' function calculates the value and derivative, as a
-- pair, of a scalar-to-scalar function.
diff2UU :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> (a, a)
diff2UU f a = derivative2 $ f (var a 0)
{-# INLINE diff2UU #-}

diff2UF :: (Functor f, Num a) => (forall s. Mode s => AD s a -> f (AD s a)) -> a -> f (a, a)
diff2UF f a = derivative2 <$> f (var a 0)
{-# INLINE diff2UF #-}

diffFU :: (Traversable f, Num a) => (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> f a
diffFU f as = unbind vs $ partialArray bounds (f vs)
    where (vs, bounds) = bind as
{-# INLINE diffFU #-}

diff2FU :: (Traversable f, Num a) => (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> (a, f a)
diff2FU f as = (primal result, unbind vs $ partialArray bounds result)
    where (vs, bounds) = bind as
          result = f vs
{-# INLINE diff2FU #-}

-- | The 'diff' function is a synonym for 'diffUU'.
diff :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> a
diff = diffUU 
{-# INLINE diff #-}

-- | The 'diff2' function is a synonym for 'diff2UU'.
diff2 :: Num a => (forall s. Mode s => AD s a -> AD s a) -> a -> (a, a)
diff2 = diff2UU
{-# INLINE diff2 #-}

-- | The 'zeroNewton' function finds a zero of a scalar function using
-- Newton's method; its output is a stream of increasingly accurate
-- results.  (Modulo the usual caveats.)
--
-- TEST CASE:
--  @take 10 $ zeroNewton (\\x->x^2-4) 1  -- converge to 2.0@
--
-- TEST CASE
--  :module Data.Complex Numeric.RAD
--  @take 10 $ zeroNewton ((+1).(^2)) (1 :+ 1)  -- converge to (0 :+ 1)@
--
zeroNewton :: Fractional a => (forall s. Mode s => AD s a -> AD s a) -> a -> [a]
zeroNewton f x0 = iterate (\x -> let (y,y') = diff2UU f x in x - y/y') x0
{-# INLINE zeroNewton #-}

-- | The 'inverseNewton' function inverts a scalar function using
-- Newton's method; its output is a stream of increasingly accurate
-- results.  (Modulo the usual caveats.)
--
-- TEST CASE:
--   @take 10 $ inverseNewton sqrt 1 (sqrt 10)  -- converge to 10@
--
inverseNewton :: Fractional a => (forall s. Mode s => AD s a -> AD s a) -> a -> a -> [a]
inverseNewton f x0 y = zeroNewton (\x -> f x - lift y) x0
{-# INLINE inverseNewton #-}

-- | The 'fixedPointNewton' function find a fixedpoint of a scalar
-- function using Newton's method; its output is a stream of
-- increasingly accurate results.  (Modulo the usual caveats.)
fixedPointNewton :: Fractional a => (forall s. Mode s => AD s a -> AD s a) -> a -> [a]
fixedPointNewton f = zeroNewton (\x -> f x - x)
{-# INLINE fixedPointNewton #-}

-- | The 'extremumNewton' function finds an extremum of a scalar
-- function using Newton's method; produces a stream of increasingly
-- accurate results.  (Modulo the usual caveats.)
extremumNewton :: Fractional a => (forall t s. (Mode t, Mode s) => AD t (AD s a) -> AD t (AD s a)) -> a -> [a]
extremumNewton f x0 = zeroNewton (diffUU f) x0
{-# INLINE extremumNewton #-}

{-
-- | The 'argminNaiveGradient' function performs a multivariate
-- optimization, based on the naive-gradient-descent in the file
-- @stalingrad\/examples\/flow-tests\/pre-saddle-1a.vlad@ from the
-- VLAD compiler Stalingrad sources.  Its output is a stream of
-- increasingly accurate results.  (Modulo the usual caveats.)  
-- This is /O(n)/ faster than 'Numeric.FAD.argminNaiveGradient'
argminNaiveGradient :: (Fractional a, Ord a) => (forall s. Mode s => [AD s a] -> AD s a) -> [a] -> [[a]]
argminNaiveGradient f x0 =
    let
        gf = grad f
        loop x fx gx eta i =
            -- should check gx = 0 here
            let
                x1 = zipWith (+) x (map ((-eta)*) gx)
                fx1 = lowerFU f x1
                gx1 = gf x1
            in
              if eta == 0 then []
              else if (fx1 > fx) then loop x fx gx (eta/2) 0
                   else if all (==0) gx then []
                        -- else if fx1 == fx then loop x1 fx1 gx1 eta (i+1)
                        else x1:(if (i==10)
                                 then loop x1 fx1 gx1 (eta*2) 0
                                 else loop x1 fx1 gx1 eta (i+1))
    in
      loop x0 (lowerFU f x0) (gf x0) 0.1 0
{-# INLINE argminNaiveGradient #-}

lowerFU :: (Functor f, Primal (AD s), Mode s, Num a) => (f (AD s a) -> AD s a) -> f a -> a
lowerFU f = primal . f . fmap lift
{-# INLINE lowerFU #-}
-}

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
partials (AD tape) = [ (id, sensitivities ! ix) | (ix, Var _ id) <- xs ]
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
    
-- | Pass variables for forward substitution
class Var t a | t -> a where
    var     :: a -> Int -> t
    fromVar :: t -> Int

    bind :: Traversable f => f a -> (f t, (Int,Int))
    unbind :: Functor f => f t -> Array Int a -> f a 
    unbindMap :: (Functor f, Num a) => f t -> IntMap a -> f a

    bind xs = (r,(0,s)) 
        where 
        (r,s) = runS (mapM freshVar xs) 0
        freshVar a = S (\s -> let s' = s + 1 in s' `seq` (var a s, s'))
    unbind xs ys = fmap (\v -> ys ! fromVar v) xs
    unbindMap xs ys = fmap (\v -> findWithDefault 0 (fromVar v) ys) xs

instance Var (Reverse a) a where
    var a v = Reverse (Var a v)
    fromVar (Reverse (Var _ v)) = v

instance Var (AD Reverse a) a where
    var a v = AD (var a v)
    fromVar (AD v) = fromVar v
