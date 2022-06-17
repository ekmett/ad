module Main (main) where

import qualified Numeric.AD.Mode.Reverse as R
import qualified Numeric.AD.Mode.Reverse.Double as RD

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Regression tests"
  [ testCase "#97" $
      assertBool "Reverse.diff and Reverse.Double.diff should behave identically" $
      nearZero $ R.diff f (0 :: Double) - RD.diff f (0 :: Double)
  ]

-- Reverse.Double +ffi initializes the tape with a block of size 4096
-- The large term in this function forces the allocation of an additional block
f :: Num a => a -> a
f = sum . replicate 5000

nearZero :: (Fractional a, Ord a) => a -> Bool
nearZero a = abs a <= 1e-12
