{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import qualified Numeric.AD.Mode.Reverse as R
import qualified Numeric.AD.Mode.Reverse.Double as RD

import Text.Printf
import Test.Tasty
import Test.Tasty.HUnit

type Diff = (forall a. Floating a => a -> a) -> Double -> (Double, Double)
type Grad = (forall a. Floating a => [a] -> a) -> [Double] -> [Double]
type Jacobian = (forall a. Floating a => [a] -> [a]) -> [Double] -> [[Double]]
type Hessian = (forall a. Floating a => [a] -> a) -> [Double] -> [[Double]]

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [
  mode "reverse" (\ f -> R.diff' f) (\ f -> R.grad f) (\ f -> R.jacobian f) (\ f -> R.hessian f),
  mode "reverse-double" (\ f -> RD.diff' f) (\ f -> RD.grad f) (\ f -> RD.jacobian f) (\ f -> RD.hessian f)]

mode :: String -> Diff -> Grad -> Jacobian -> Hessian -> TestTree
mode name diff grad jacobian hessian = testGroup name [basic diff grad jacobian hessian, issue97 diff, issue104 diff grad]

basic :: Diff -> Grad -> Jacobian -> Hessian -> TestTree
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
issue97 :: Diff -> TestTree
issue97 diff = testCase "issue-97" $ expect eq 5000 $ snd $ diff f 0 where f = sum . replicate 5000

issue104 :: Diff -> Grad -> TestTree
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
