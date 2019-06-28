{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ParallelListComp #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2010-2015
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------

module Numeric.AD.Newton
  (
  -- * Newton's Method (Forward AD)
    findZero
  , findZeroNoEq
  , inverse
  , inverseNoEq
  , fixedPoint
  , fixedPointNoEq
  , extremum
  , extremumNoEq
  -- * Gradient Ascent/Descent (Reverse AD)
  , gradientDescent, constrainedDescent, CC(..), eval
  , gradientAscent
  , conjugateGradientDescent
  , conjugateGradientAscent
  , stochasticGradientDescent
  ) where

#if __GLASGOW_HASKELL__ < 710
import Data.Foldable (Foldable, all, sum)
#else
import Data.Foldable (all, sum)
#endif
import Data.Reflection (Reifies)
import Data.Traversable
import Numeric.AD.Internal.Combinators
import Numeric.AD.Internal.Forward (Forward)
import Numeric.AD.Internal.On
import Numeric.AD.Internal.Or
import Numeric.AD.Internal.Reverse (Reverse, Tape)
import Numeric.AD.Internal.Type (AD(..))
import Numeric.AD.Mode
import Numeric.AD.Mode.Reverse as Reverse (gradWith, gradWith', grad')
import Numeric.AD.Rank1.Kahn as Kahn (Kahn, grad)
import qualified Numeric.AD.Rank1.Newton as Rank1
import Prelude hiding (all, mapM, sum)

-- $setup
-- >>> import Data.Complex

-- | The 'findZero' function finds a zero of a scalar function using
-- Newton's method; its output is a stream of increasingly accurate
-- results.  (Modulo the usual caveats.) If the stream becomes constant
-- ("it converges"), no further elements are returned.
--
-- Examples:
--
-- >>> take 10 $ findZero (\x->x^2-4) 1
-- [1.0,2.5,2.05,2.000609756097561,2.0000000929222947,2.000000000000002,2.0]
--
-- >>> last $ take 10 $ findZero ((+1).(^2)) (1 :+ 1)
-- 0.0 :+ 1.0
findZero :: (Fractional a, Eq a) => (forall s. AD s (Forward a) -> AD s (Forward a)) -> a -> [a]
findZero f = Rank1.findZero (runAD.f.AD)
{-# INLINE findZero #-}

-- | The 'findZeroNoEq' function behaves the same as 'findZero' except that it
-- doesn't truncate the list once the results become constant. This means it
-- can be used with types without an 'Eq' instance.
findZeroNoEq :: Fractional a => (forall s. AD s (Forward a) -> AD s (Forward a)) -> a -> [a]
findZeroNoEq f = Rank1.findZeroNoEq (runAD.f.AD)
{-# INLINE findZeroNoEq #-}

-- | The 'inverse' function inverts a scalar function using
-- Newton's method; its output is a stream of increasingly accurate
-- results.  (Modulo the usual caveats.) If the stream becomes
-- constant ("it converges"), no further elements are returned.
--
-- Example:
--
-- >>> last $ take 10 $ inverse sqrt 1 (sqrt 10)
-- 10.0
inverse :: (Fractional a, Eq a) => (forall s. AD s (Forward a) -> AD s (Forward a)) -> a -> a -> [a]
inverse f = Rank1.inverse (runAD.f.AD)
{-# INLINE inverse  #-}

-- | The 'inverseNoEq' function behaves the same as 'inverse' except that it
-- doesn't truncate the list once the results become constant. This means it
-- can be used with types without an 'Eq' instance.
inverseNoEq :: Fractional a => (forall s. AD s (Forward a) -> AD s (Forward a)) -> a -> a -> [a]
inverseNoEq f = Rank1.inverseNoEq (runAD.f.AD)
{-# INLINE inverseNoEq #-}

-- | The 'fixedPoint' function find a fixedpoint of a scalar
-- function using Newton's method; its output is a stream of
-- increasingly accurate results.  (Modulo the usual caveats.)
--
-- If the stream becomes constant ("it converges"), no further
-- elements are returned.
--
-- >>> last $ take 10 $ fixedPoint cos 1
-- 0.7390851332151607
fixedPoint :: (Fractional a, Eq a) => (forall s. AD s (Forward a) -> AD s (Forward a)) -> a -> [a]
fixedPoint f = Rank1.fixedPoint (runAD.f.AD)
{-# INLINE fixedPoint #-}

-- | The 'fixedPointNoEq' function behaves the same as 'fixedPoint' except that
-- it doesn't truncate the list once the results become constant. This means it
-- can be used with types without an 'Eq' instance.
fixedPointNoEq :: Fractional a => (forall s. AD s (Forward a) -> AD s (Forward a)) -> a -> [a]
fixedPointNoEq f = Rank1.fixedPointNoEq (runAD.f.AD)
{-# INLINE fixedPointNoEq #-}

-- | The 'extremum' function finds an extremum of a scalar
-- function using Newton's method; produces a stream of increasingly
-- accurate results.  (Modulo the usual caveats.) If the stream
-- becomes constant ("it converges"), no further elements are returned.
--
-- >>> last $ take 10 $ extremum cos 1
-- 0.0
extremum :: (Fractional a, Eq a) => (forall s. AD s (On (Forward (Forward a))) -> AD s (On (Forward (Forward a)))) -> a -> [a]
extremum f = Rank1.extremum (runAD.f.AD)
{-# INLINE extremum #-}

-- | The 'extremumNoEq' function behaves the same as 'extremum' except that it
-- doesn't truncate the list once the results become constant. This means it
-- can be used with types without an 'Eq' instance.
extremumNoEq :: Fractional a => (forall s. AD s (On (Forward (Forward a))) -> AD s (On (Forward (Forward a)))) -> a -> [a]
extremumNoEq f = Rank1.extremumNoEq (runAD.f.AD)
{-# INLINE extremumNoEq #-}

-- | The 'gradientDescent' function performs a multivariate
-- optimization, based on the naive-gradient-descent in the file
-- @stalingrad\/examples\/flow-tests\/pre-saddle-1a.vlad@ from the
-- VLAD compiler Stalingrad sources.  Its output is a stream of
-- increasingly accurate results.  (Modulo the usual caveats.)
--
-- It uses reverse mode automatic differentiation to compute the gradient.
gradientDescent :: (Traversable f, Fractional a, Ord a) => (forall s. Reifies s Tape => f (Reverse s a) -> Reverse s a) -> f a -> [f a]
gradientDescent f x0 = go x0 fx0 xgx0 0.1 (0 :: Int)
  where
    (fx0, xgx0) = Reverse.gradWith' (,) f x0
    go x fx xgx !eta !i
      | eta == 0     = [] -- step size is 0
      | fx1 > fx     = go x fx xgx (eta/2) 0 -- we stepped too far
      | zeroGrad xgx = [] -- gradient is 0
      | otherwise    = x1 : if i == 10
                            then go x1 fx1 xgx1 (eta*2) 0
                            else go x1 fx1 xgx1 eta (i+1)
      where
        zeroGrad = all (\(_,g) -> g == 0)
        x1 = fmap (\(xi,gxi) -> xi - eta * gxi) xgx
        (fx1, xgx1) = Reverse.gradWith' (,) f x1
{-# INLINE gradientDescent #-}

data SEnv (f :: * -> *) a = SEnv { sValue :: a, origEnv :: f a }
  deriving (Functor, Foldable, Traversable)

-- | Convex constraint, CC, is a GADT wrapper that hides the existential
-- ('s') which is so prevalent in the rest of the API.  This is an
-- engineering convenience for managing the skolems.
data CC f a where
  CC :: forall f a. (forall s. Reifies s Tape => f (Reverse s a) -> Reverse s a) -> CC f a

-- |@constrainedDescent obj fs env@ optimizes the convex function @obj@
-- subject to the convex constraints @f <= 0@ where @f `elem` fs@. This is
-- done using a log barrier to model constraints (i.e. Boyd, Chapter 11.3).
-- The returned optimal point for the objective function must satisfy @fs@,
-- but the initial environment, @env@, needn't be feasible.
constrainedDescent :: forall f a. (Traversable f, RealFloat a, Floating a, Ord a)
                   => (forall s. Reifies s Tape => f (Reverse s a)
                                                -> Reverse s a)
                   -> [CC f a]
                   -> f a
                   -> [(a,f a)]
constrainedDescent objF [] env =
  map (\x -> (eval objF x, x)) (gradientDescent objF env)
constrainedDescent objF cs env =
    let s0       = 1 + maximum [eval c env | CC c <- cs]
        -- ^ s0 = max ( f_i(0) )
        cs'      = [CC (\(SEnv sVal rest) -> c rest - sVal) | CC c <- cs]
        -- ^ f_i' = f_i - s0  and thus f_i' <= 0
        envS     = SEnv s0 env
        -- feasible point for f_i', use gd to find feasiblity for f_i
        cc       = constrainedConvex' (CC sValue) cs' envS ((<=0) . sValue)
    in case dropWhile ((0 <) . fst) (take (2^(20::Int)) cc) of
        []                  -> []
        (_,envFeasible) : _ ->
            constrainedConvex' (CC objF) cs (origEnv envFeasible) (const True)
{-# INLINE constrainedDescent #-}

eval :: (Traversable f, Fractional a, Ord a) => (forall s. Reifies s Tape => f (Reverse s a) -> Reverse s a) -> f a -> a
eval f e = fst (grad' f e)
{-# INLINE eval #-}

-- | Like 'constrainedDescent' except the initial point must be feasible.
constrainedConvex' :: forall f a. (Traversable f, RealFloat a, Floating a, Ord a)
                   => CC f a
                   -> [CC f a]
                   -> f a
                   -> (f a -> Bool)
                   -> [(a,f a)]
constrainedConvex' objF cs env term =
  -- 1. Transform cs using a log barrier with increasing t values.
  let os   = map (mkOpt objF cs) tValues
  -- 2. Iteratively run gradientDescent on each os.
      envs =  [(undefined,env)] :
              [gD (snd $ last e) o
                          | o  <- os
                          | e  <- limEnvs
                          ]
      -- Obtain a finite number of elements from the initial len tValues - 1 lists.
      limEnvs = zipWith id nrSteps envs
  in dropWhile (not . term . snd) (concat $ drop 1 limEnvs)
 where
  tValues = map realToFrac $ take 64 $ iterate (*2) (2 :: a)
  nrSteps = [take 20 | _ <- [1..length tValues]] ++ [id]
  -- | `gD f e` is gradient descent with the evaulated result
  gD e (CC f)  = (eval f e, e) :
                 map (\x -> (eval f x, x)) (gradientDescent f e)
{-# INLINE constrainedConvex' #-}

-- @mkOpt u fs t@ converts an inequality convex problem (@u,fs@) into an
-- unconstrained convex problem using log barrier @u + -(1/t)log(-f_i)@.
-- As @t@ increases the approximation is more accurate but the gradient
-- decreases, making the gradient descent more expensive.
mkOpt :: forall f a. (Traversable f, RealFloat a, Floating a, Ord a)
      => CC f a -> [CC f a]
      -> a -> CC f a
mkOpt (CC o) xs t = CC (\e -> o e + sum (map (\(CC c) -> iHat t c e) xs))
{-# INLINE mkOpt #-}

iHat :: forall a f. (Traversable f, RealFloat a, Floating a, Ord a)
     => a
     -> (forall s. Reifies s Tape => f (Reverse s a) -> Reverse s a)
     -> (forall s. Reifies s Tape => f (Reverse s a) -> Reverse s a)
iHat t c e =
   let r = c e
   in if r >= 0 || isNaN r
        then 1  / 0
        else (-1 / auto t) * log( - (c  e))
{-# INLINE iHat #-}

-- | The 'stochasticGradientDescent' function approximates
-- the true gradient of the constFunction by a gradient at
-- a single example. As the algorithm sweeps through the training
-- set, it performs the update for each training example.
--
-- It uses reverse mode automatic differentiation to compute the gradient
-- The learning rate is constant through out, and is set to 0.001
stochasticGradientDescent :: (Traversable f, Fractional a, Ord a)
  => (forall s. Reifies s Tape => e -> f (Reverse s a) -> Reverse s a)
  -> [e]
  -> f a
  -> [f a]
stochasticGradientDescent errorSingle d0 x0 = go xgx0 0.001 dLeft
  where
    dLeft = tail $ cycle d0
    xgx0 = Reverse.gradWith (,) (errorSingle (head d0)) x0
    go xgx !eta d
      | eta ==0       = []
      | otherwise     = x1 : go xgx1 eta (tail d)
      where
        x1 = fmap (\(xi, gxi) -> xi - eta * gxi) xgx
        (_, xgx1) = Reverse.gradWith' (,) (errorSingle (head d)) x1
{-# INLINE stochasticGradientDescent #-}

-- | Perform a gradient descent using reverse mode automatic differentiation to compute the gradient.
gradientAscent :: (Traversable f, Fractional a, Ord a) => (forall s. Reifies s Tape => f (Reverse s a) -> Reverse s a) -> f a -> [f a]
gradientAscent f = gradientDescent (negate . f)
{-# INLINE gradientAscent #-}

-- | Perform a conjugate gradient descent using reverse mode automatic differentiation to compute the gradient, and using forward-on-forward mode for computing extrema.
--
-- >>> let sq x = x * x
-- >>> let rosenbrock [x,y] = sq (1 - x) + 100 * sq (y - sq x)
-- >>> rosenbrock [0,0]
-- 1
-- >>> rosenbrock (conjugateGradientDescent rosenbrock [0, 0] !! 5) < 0.1
-- True
conjugateGradientDescent
  :: (Traversable f, Ord a, Fractional a)
  => (forall s. Chosen s => f (Or s (On (Forward (Forward a))) (Kahn a)) -> Or s (On (Forward (Forward a))) (Kahn a))
  -> f a -> [f a]
conjugateGradientDescent f = conjugateGradientAscent (negate . f)
{-# INLINE conjugateGradientDescent #-}

lfu :: Functor f => (f (Or F a b) -> Or F a b) -> f a -> a
lfu f = runL . f . fmap L

rfu :: Functor f => (f (Or T a b) -> Or T a b) -> f b -> b
rfu f = runR . f . fmap R

-- | Perform a conjugate gradient ascent using reverse mode automatic differentiation to compute the gradient.
conjugateGradientAscent
  :: (Traversable f, Ord a, Fractional a)
  => (forall s. Chosen s => f (Or s (On (Forward (Forward a))) (Kahn a)) -> Or s (On (Forward (Forward a))) (Kahn a))
  -> f a -> [f a]
conjugateGradientAscent f x0 = takeWhile (all (\a -> a == a)) (go x0 d0 d0 delta0)
  where
    dot x y = sum $ zipWithT (*) x y
    d0 = Kahn.grad (rfu f) x0
    delta0 = dot d0 d0
    go xi _ri di deltai = xi : go xi1 ri1 di1 deltai1
      where
        ai = last $ take 20 $ Rank1.extremum (\a -> lfu f $ zipWithT (\x d -> auto x + a * auto d) xi di) 0
        xi1 = zipWithT (\x d -> x + ai*d) xi di
        ri1 = Kahn.grad (rfu f) xi1
        deltai1 = dot ri1 ri1
        bi1 = deltai1 / deltai
        di1 = zipWithT (\r d -> r + bi1 * d) ri1 di
{-# INLINE conjugateGradientAscent #-}
