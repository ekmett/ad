{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Numeric
import qualified Numeric.AD.Mode.Forward as F
import qualified Numeric.AD.Mode.Forward.Double as FD
import qualified Numeric.AD.Mode.Reverse as R
import qualified Numeric.AD.Mode.Reverse.Double as RD

import Text.Printf
import Test.Tasty
import Test.Tasty.HUnit

type Diff' = (forall a. Floating a => a -> a) -> Double -> (Double, Double)
type Grad = (forall a. Floating a => [a] -> a) -> [Double] -> [Double]
type Jacobian = (forall a. Floating a => [a] -> [a]) -> [Double] -> [[Double]]
type Hessian = (forall a. Floating a => [a] -> a) -> [Double] -> [[Double]]

main :: IO ()
main = defaultMain tests

-- TODO: the forward-double tests are currently failing due to discrepancies between the modes
--       see also https://github.com/ekmett/ad/issues/109 and https://github.com/ekmett/ad/pull/110
tests :: TestTree
tests = testGroup "tests" [
  mode "forward" (\ f -> F.diff' f) (\ f -> F.grad f) (\ f -> F.jacobian f) (\ f -> F.jacobian $ F.grad f),
  --mode "forward-double" (\ f -> FD.diff' f) (\ f -> FD.grad f) (\ f -> FD.jacobian f) (\ f -> FD.jacobian $ F.grad f),
  mode "reverse" (\ f -> R.diff' f) (\ f -> R.grad f) (\ f -> R.jacobian f) (\ f -> R.hessian f),
  mode "reverse-double" (\ f -> RD.diff' f) (\ f -> RD.grad f) (\ f -> RD.jacobian f) (\ f -> RD.hessian f)]

mode :: String -> Diff' -> Grad -> Jacobian -> Hessian -> TestTree
mode name diff grad jacobian hessian = testGroup name [
  basic diff grad jacobian hessian,
  issue97 diff,
  issue104 diff grad,
  issue108 diff]

basic :: Diff' -> Grad -> Jacobian -> Hessian -> TestTree
basic diff grad jacobian hessian = testGroup "basic" [tdiff, tgrad, tjacobian, thessian] where
  tdiff = testCase "diff" $ do
    expect (list eq) [11, 5.5, 3, 3.5, 7, 13.5, 23, 35.5, 51] $ snd . diff p <$> [-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2]
    expect (list eq) [nan, inf, 1, 0.5, 0.25] $ snd . diff sqrt <$> [-1, 0, 0.25, 1, 4]
    expect (list eq) [1, 0, 1] $ [snd . diff sin, snd . diff cos, snd . diff tan] <*> [0]
    expect (list eq) [-1, 0, 1] $ snd . diff abs <$> [-1, 0, 1]
    expect (list eq) [1, exp 1, inf, 1] $ [snd . diff exp, snd . diff log] <*> [0, 1]
  tgrad = testCase "grad" $ do
    expect (list eq) [2, 1, 1] $ grad f [1, 2, 3]
    expect (list eq) [1, 0.25] $ grad h [2, 8]
    expect (list eq) [0, nan] $ grad power [0, 2]
  tjacobian = testCase "jacobian" $ do
    expect (list $ list eq) [[0, 1], [1, 0], [1, 2]] $ jacobian g [2, 1]
  thessian = testCase "hessian" $ do
    expect (list $ list eq) [[0, 1, 0], [1, 0, 0], [0, 0, 0]] $ hessian f [1, 2, 3]
    expect (list $ list eq) [[0, 0], [0, 0]] $ hessian sum [1, 2]
    expect (list $ list eq) [[0, 1], [1, 0]] $ hessian product [1, 2]
    expect (list $ list eq) [[2, 1], [1, 0]] $ hessian power [1, 2]
  sum [x, y] = x + y
  product [x, y] = x * y
  power [x, y] = x ** y
  f [x, y, z] = x * y + z
  g [x, y] = [y, x, x * y]
  h [x, y] = sqrt $ x * y
  p x = 12 + 7 * x + 5 * x ^ 2 + 2 * x ^ 3

-- Reverse.Double +ffi initializes the tape with a block of size 4096
-- The large term in this function forces the allocation of an additional block
issue97 :: Diff' -> TestTree
issue97 diff = testCase "issue-97" $ expect eq 5000 $ snd $ diff f 0 where f = sum . replicate 5000

issue104 :: Diff' -> Grad -> TestTree
issue104 diff grad = testGroup "issue-104" [inside, outside] where
  inside = testGroup "inside" [tdiff, tgrad] where
    tdiff = testCase "diff" $ do
      expect (list eq) [nan, nan] $ snd . diff (0 `f`) <$> [0, 1]
      expect (list eq) [inf, 0.5] $ snd . diff (1 `f`) <$> [0, 1]
      expect (list eq) [nan, nan] $ snd . diff (`f` 0) <$> [0, 1]
      expect (list eq) [inf, 0.5] $ snd . diff (`f` 1) <$> [0, 1]
    tgrad = testCase "grad" $ do
      expect (list eq) [nan, nan] $ grad (binary f) [0, 0]
      expect (list eq) [nan, inf] $ grad (binary f) [1, 0]
      expect (list eq) [inf, nan] $ grad (binary f) [0, 1]
      expect (list eq) [0.5, 0.5] $ grad (binary f) [1, 1]
    f x y = sqrt $ x * y -- grad f [x, y] = [y / (2 * f x y), x / (2 * f x y)]
  outside = testGroup "outside" [tdiff, tgrad] where
    tdiff = testCase "diff" $ do
      expect (list eq) [nan, 0.0] $ snd . diff (0 `f`) <$> [0, 1]
      expect (list eq) [inf, 0.5] $ snd . diff (1 `f`) <$> [0, 1]
      expect (list eq) [nan, 0.0] $ snd . diff (`f` 0) <$> [0, 1]
      expect (list eq) [inf, 0.5] $ snd . diff (`f` 1) <$> [0, 1]
    tgrad = testCase "grad" $ do
      expect (list eq) [nan, nan] $ grad (binary f) [0, 0]
      expect (list eq) [0.0, inf] $ grad (binary f) [1, 0]
      expect (list eq) [inf, 0.0] $ grad (binary f) [0, 1]
      expect (list eq) [0.5, 0.5] $ grad (binary f) [1, 1]
    f x y = sqrt x * sqrt y -- grad f [x, y] = [sqrt y / 2 sqrt x, sqrt x / 2 sqrt y]
  binary f [x, y] = f x y

issue108 :: Diff' -> TestTree
issue108 diff = testGroup "issue-108" [tlog1p, texpm1, tlog1pexp, tlog1mexp] where
  tlog1p = testCase "log1p" $ do
    equal (-inf, inf) $ diff log1p (-1)
    equal (-1.0000000000000007e-15, 1.000000000000001) $ diff log1p (-1e-15)
    equal (-1e-20, 1) $ diff log1p (-1e-20)
    equal (0, 1) $ diff log1p 0
    equal (1e-20, 1) $ diff log1p 1e-20
    equal (9.999999999999995e-16, 0.9999999999999989) $ diff log1p 1e-15
    equal (0.6931471805599453, 0.5) $ diff log1p 1
  texpm1 = testCase "expm1" $ do
    equal (-0.6321205588285577, 0.36787944117144233) $ diff expm1 (-1)
    equal (-9.999999999999995e-16, 0.999999999999999) $ diff expm1 (-1e-15)
    equal (-1e-20, 1) $ diff expm1 (-1e-20)
    equal (0, 1) $ diff expm1 0
    equal (1e-20, 1) $ diff expm1 1e-20
    equal (1.0000000000000007e-15, 1.000000000000001) $ diff expm1 1e-15
    equal (1.718281828459045, 2.718281828459045) $ diff expm1 1
  tlog1pexp = testCase "log1pexp" $ do
    equal (0, 0) $ diff log1pexp (-1000)
    equal (3.720075976020836e-44, 3.7200759760208356e-44) $ diff log1pexp (-100)
    equal (0.31326168751822286, 0.2689414213699951) $ diff log1pexp (-1)
    equal (0.6931471805599453, 0.5) $ diff log1pexp 0
    equal (1.3132616875182228, 0.7310585786300049) $ diff log1pexp 1
    equal (100, 1) $ diff log1pexp 100
    equal (1000, 1) $ diff log1pexp 1000
  tlog1mexp = testCase "log1mexp" $ do
    equal (-0, -0) $ diff log1mexp (-1000)
    equal (-3.720075976020836e-44, -3.7200759760208356e-44) $ diff log1mexp (-100)
    equal (-0.45867514538708193, -0.5819767068693265) $ diff log1mexp (-1)
    equal (-0.9327521295671886, -1.5414940825367982) $ diff log1mexp (-0.5)
    equal (-2.3521684610440907, -9.50833194477505) $ diff log1mexp (-0.1)
    equal (-34.538776394910684, -9.999999999999994e14) $ diff log1mexp (-1e-15)
    equal (-46.051701859880914, -1e20) $ diff log1mexp (-1e-20)
  equal = expect $ \ (a, b) (c, d) -> eq a c && eq b d

-- TODO: ideally, we would consider `0` and `-0` to be different
--       however, zero signedness is currently not reliably propagated through some modes
--       see also https://github.com/ekmett/ad/issues/109 and https://github.com/ekmett/ad/pull/110
eq :: Double -> Double -> Bool
eq a b = isNaN a && isNaN b || a == b

list :: (a -> a -> Bool) -> [a] -> [a] -> Bool
list eq as bs = length as == length bs && and (zipWith eq as bs)

expect :: HasCallStack => Show a => (a -> a -> Bool) -> a -> a -> Assertion
expect eq a b = eq a b @? printf "expected %s but got %s" (show a) (show b)

nan :: Double
nan = 0 / 0

inf :: Double
inf = 1 / 0
