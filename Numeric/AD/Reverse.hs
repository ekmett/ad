{-# LANGUAGE Rank2Types, TemplateHaskell #-}
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
    ( 
    -- * Gradient
      grad
    , grad2
    -- * Jacobian
    , jacobian
    , jacobian2
    -- * Derivatives
    , diffUU
    , diff2UU
    , diffFU
    , diff2FU
    , diffUF
    , diff2UF
    -- * Synonyms
    , diff
    , diff2
    -- * Exposed Types
    , AD(..)
    , Mode(..)
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
import Numeric.AD.Reverse.Internal

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

-- | The 'jacobian2' function calculates both the result and the Jacobian of a nonscalar-to-nonscalar function, using @m@ invocations of reverse AD,
-- where @m@ is the output dimensionality. Applying @fmap snd@ to the result will recover the result of 'jacobian'
jacobian2 :: (Traversable f, Functor g, Num a) => (forall s. Mode s => f (AD s a) -> g (AD s a)) -> f a -> g (a, f a)
jacobian2 f as = row <$> f vs where 
    (vs, bounds) = bind as
    row a = (primal a, unbind vs (partialArray bounds a))
{-# INLINE jacobian2 #-}

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
