{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import qualified Numeric.AD.Mode.Reverse as R
import qualified Numeric.AD.Mode.Reverse.Double as RD

import Text.Printf
import Test.Tasty
import Test.Tasty.HUnit

type Diff = (forall a. Floating a => a -> a) -> Double -> Double
type Grad = (forall a. Floating a => [a] -> a) -> [Double] -> [Double]
type Jacobian = (forall a. Floating a => [a] -> [a]) -> [Double] -> [[Double]]
type Hessian = (forall a. Floating a => [a] -> a) -> [Double] -> [[Double]]

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [
  mode "reverse" (\ f -> R.diff f) (\ f -> R.grad f) (\ f -> R.jacobian f) (\ f -> R.hessian f),
  mode "reverse-double" (\ f -> RD.diff f) (\ f -> RD.grad f) (\ f -> RD.jacobian f) (\ f -> RD.hessian f)]

mode :: String -> Diff -> Grad -> Jacobian -> Hessian -> TestTree
mode name diff grad jacobian hessian = testGroup name [basic diff grad jacobian hessian, issue97 diff, issue104 diff grad]

basic :: Diff -> Grad -> Jacobian -> Hessian -> TestTree
basic diff grad jacobian hessian = testGroup "basic" [tdiff, tgrad, tjacobian, thessian] where
  tdiff = testCase "diff" $ do
    assertNearList [11, 5.5, 3, 3.5, 7, 13.5, 23, 35.5, 51] $ diff p <$> [-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2]
    assertNearList [nan, inf, 1, 0.5, 0.25] $ diff sqrt <$> [-1, 0, 0.25, 1, 4]
    assertNearList [1, 0, 1] $ [diff sin, diff cos, diff tan] <*> [0]
    assertNearList [-1, 0, 1] $ diff abs <$> [-1, 0, 1]
    assertNearList [1, exp 1, inf, 1] $ [diff exp, diff log] <*> [0, 1]
  tgrad = testCase "grad" $ do
    assertNearList [2, 1, 1] $ grad f [1, 2, 3]
    assertNearList [1, 0.25] $ grad h [2, 8]
    assertNearList [0, nan] $ grad power [0, 2]
  tjacobian = testCase "jacobian" $ do
    assertNearMatrix [[0, 1], [1, 0], [1, 2]] $ jacobian g [2, 1]
  thessian = testCase "hessian" $ do
    assertNearMatrix [[0, 1, 0], [1, 0, 0], [0, 0, 0]] $ hessian f [1, 2, 3]
    assertNearMatrix [[0, 0], [0, 0]] $ hessian sum [1, 2]
    assertNearMatrix [[0, 1], [1, 0]] $ hessian product [1, 2]
    assertNearMatrix [[2, 1], [1, 0]] $ hessian power [1, 2]
  sum = \ [x, y] -> x + y
  product = \ [x, y] -> x * y
  power = \ [x, y] -> x ** y
  f = \ [x, y, z] -> x * y + z
  g = \ [x, y] -> [y, x, x * y]
  h = \ [x, y] -> sqrt $ x * y
  p = \ x -> 12 + 7 * x + 5 * x ^ 2 + 2 * x ^ 3

-- Reverse.Double +ffi initializes the tape with a block of size 4096
-- The large term in this function forces the allocation of an additional block
issue97 :: Diff -> TestTree
issue97 diff = testCase "issue-97" $ assertNear 5000 $ diff f 0 where f = sum . replicate 5000

issue104 :: Diff -> Grad -> TestTree
issue104 diff grad = testGroup "issue-104" [inside, outside] where
  inside = testGroup "inside" [tdiff, tgrad] where
    tdiff = testCase "diff" $ do
      assertNearList [nan, nan] $ diff (0 `f`) <$> [0, 1]
      assertNearList [inf, 0.5] $ diff (1 `f`) <$> [0, 1]
      assertNearList [nan, nan] $ diff (`f` 0) <$> [0, 1]
      assertNearList [inf, 0.5] $ diff (`f` 1) <$> [0, 1]
    tgrad = testCase "grad" $ do
      assertNearList [nan, nan] $ grad (binary f) [0, 0]
      assertNearList [nan, inf] $ grad (binary f) [1, 0]
      assertNearList [inf, nan] $ grad (binary f) [0, 1]
      assertNearList [0.5, 0.5] $ grad (binary f) [1, 1]
    f x y = sqrt $ x * y -- grad f [x, y] = [y / (2 * f x y), x / (2 * f x y)]
  outside = testGroup "outside" [tdiff, tgrad] where
    tdiff = testCase "diff" $ do
      assertNearList [nan, 0.0] $ diff (0 `f`) <$> [0, 1]
      assertNearList [inf, 0.5] $ diff (1 `f`) <$> [0, 1]
      assertNearList [nan, 0.0] $ diff (`f` 0) <$> [0, 1]
      assertNearList [inf, 0.5] $ diff (`f` 1) <$> [0, 1]
    tgrad = testCase "grad" $ do
      assertNearList [nan, nan] $ grad (binary f) [0, 0]
      assertNearList [0.0, inf] $ grad (binary f) [1, 0]
      assertNearList [inf, 0.0] $ grad (binary f) [0, 1]
      assertNearList [0.5, 0.5] $ grad (binary f) [1, 1]
    f x y = sqrt x * sqrt y -- grad f [x, y] = [sqrt y / 2 sqrt x, sqrt x / 2 sqrt y]
  binary f = \ [x, y] -> f x y

near :: Double -> Double -> Bool
near a b = isNaN a && isNaN b || a == b

nearList :: [Double] -> [Double] -> Bool
nearList as bs = length as == length bs && and (zipWith near as bs)

nearMatrix :: [[Double]] -> [[Double]] -> Bool
nearMatrix as bs = length as == length bs && and (zipWith nearList as bs)

assertNear :: Double -> Double -> Assertion
assertNear a b = near a b @? expect a b

assertNearList :: [Double] -> [Double] -> Assertion
assertNearList a b = nearList a b @? expect a b

assertNearMatrix :: [[Double]] -> [[Double]] -> Assertion
assertNearMatrix a b = nearMatrix a b @? expect a b

expect :: Show a => a -> a -> String
expect a b = printf "expected %s but got %s" (show a) (show b)

nan :: Double
nan = 0 / 0

inf :: Double
inf = 1 / 0
