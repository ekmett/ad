{-# LANGUAGE RankNTypes #-}

module AdditionalTests (additionalTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import qualified Numeric.AD as AD
import qualified Numeric.AD.Double as ADD
import qualified Numeric.AD.Jet as Jet
import qualified Numeric.AD.Mode.Dense as D
import qualified Numeric.AD.Mode.Kahn as K
import qualified Numeric.AD.Mode.Kahn.Double as KD
import qualified Numeric.AD.Mode.Sparse as S
import qualified Numeric.AD.Mode.Sparse.Double as SD
import qualified Numeric.AD.Mode.Tower as T
import qualified Numeric.AD.Mode.Tower.Double as TD
import qualified Numeric.AD.Rank1.Dense as RD
import qualified Numeric.AD.Rank1.Forward as RF
import qualified Numeric.AD.Rank1.Kahn as RK
import qualified Numeric.AD.Rank1.Sparse as RS
import qualified Numeric.AD.Rank1.Tower as RT

additionalTests :: TestTree
additionalTests = testGroup "additional"
  [ modeCoverage
  , combinatorCoverage
  , nanInfCoverage
  , propertyCoverage
  ]

modeCoverage :: TestTree
modeCoverage = testGroup "mode-coverage"
  [ kahnCoverage
  , sparseCoverage
  , denseCoverage
  , towerCoverage
  , rank1Coverage
  , mixedCoverage
  , doubleCoverage
  ]

kahnCoverage :: TestTree
kahnCoverage = testGroup "kahn"
  [ testCase "diff" $ expect eq 3 $ K.diff (\x -> x * x + x) 1
  , testCase "grad" $ expect (list eq) [2, 1] $ K.grad f2 [1, 2]
  , testCase "jacobian" $ expect (list (list eq)) [[1, 1], [2, 1]] $ K.jacobian g2 [1, 2]
  , testCase "hessian" $ expect (list (list eq)) [[0, 1], [1, 0]] $ K.hessian (\[x, y] -> x * y) [1, 2]
  ]

sparseCoverage :: TestTree
sparseCoverage = testGroup "sparse"
  [ testCase "grad-single" $ expect (list eq) [3] $ S.grad (\[x] -> x * x + x) [1]
  , testCase "grad" $ expect (list eq) [2, 1] $ S.grad f2 [1, 2]
  , testCase "jacobian" $ expect (list (list eq)) [[1, 1], [2, 1]] $ S.jacobian g2 [1, 2]
  , testCase "hessian" $ expect (list (list eq)) [[0, 1], [1, 0]] $ S.hessian (\[x, y] -> x * y) [1, 2]
  ]

denseCoverage :: TestTree
denseCoverage = testGroup "dense"
  [ testCase "grad" $ expect (list eq) [2, 1] $ D.grad f2 [1, 2]
  , testCase "jacobian" $ expect (list (list eq)) [[1, 1], [2, 1]] $ D.jacobian g2 [1, 2]
  ]

towerCoverage :: TestTree
towerCoverage = testGroup "tower"
  [ testCase "diffs" $ expect (list eq) [8, 12, 12, 6] $
      take 4 (T.diffs (\x -> x ^ (3 :: Int)) 2)
  , testCase "diffs0" $ expect (list eq) [0, 1, 0, 0] $
      take 4 (T.diffs0 (\x -> x) 0)
  , testCase "diffsF" $ expect (list (list eq)) [[2, 1], [4, 4, 2]] $
      fmap (take 3) (T.diffsF (\x -> [x, x * x]) 2)
  , testCase "diffs0F" $ expect (list (list eq)) [[0, 1, 0, 0]] $
      fmap (take 4) (T.diffs0F (\x -> [x]) 0)
  , testCase "du" $ expect eq 2 $
      T.du (\[x, y] -> x * y) [(1, 1), (2, 0)]
  , testCase "duF" $ expect (list eq) [1, 2] $
      T.duF (\[x, y] -> [x + y, x * y]) [(1, 1), (2, 0)]
  ]

rank1Coverage :: TestTree
rank1Coverage = testGroup "rank1"
  [ testCase "forward-diff" $ expect eq 3 $ RF.diff (\x -> x * x + x) 1
  , testCase "kahn-grad" $ expect (list eq) [2, 1] $ RK.grad f2 [1, 2]
  , testCase "sparse-jacobian" $ expect (list (list eq)) [[1, 1], [2, 1]] $ RS.jacobian g2 [1, 2]
  , testCase "dense-grad" $ expect (list eq) [2, 1] $ RD.grad f2 [1, 2]
  , testCase "tower-diffs" $ expect (list eq) [8, 12, 12] $
      take 3 (RT.diffs (\x -> x ^ (3 :: Int)) 2)
  ]

mixedCoverage :: TestTree
mixedCoverage = testGroup "numeric-ad"
  [ testCase "diff" $ expect eq 3 $ AD.diff (\x -> x * x + x) 1
  , testCase "grad" $ expect (list eq) [2, 1] $ AD.grad f2 [1, 2]
  , testCase "jacobian" $ expect (list (list eq)) [[1, 1], [2, 1]] $ AD.jacobian g2 [1, 2]
  , testCase "hessian" $ expect (list (list eq)) [[0, 1], [1, 0]] $ AD.hessian (\[x, y] -> x * y) [1, 2]
  , testCase "grads-jet" $ do
      let j = Jet.jet $ AD.grads (\[x, y] -> x * y) [1, 2]
      expect eq 2 $ Jet.headJet j
      expect (list eq) [2, 1] $ Jet.headJet $ Jet.tailJet j
  ]

doubleCoverage :: TestTree
doubleCoverage = testGroup "double"
  [ testCase "kahn" $ expect eq 3 $ KD.diff (\x -> x * x + x) 1
  , testCase "sparse" $ expect (list eq) [2, 1] $ SD.grad f2 [1, 2]
  , testCase "tower" $ expect (list eq) [8, 12, 12] $
      take 3 (TD.diffs (\x -> x ^ (3 :: Int)) 2)
  , testCase "numeric-ad-double" $ expect (list eq) [2, 1] $ ADD.grad f2 [1, 2]
  ]

combinatorCoverage :: TestTree
combinatorCoverage = testGroup "combinators"
  [ testCase "diff'" $ expect (pair eq) (2, 3) $ AD.diff' (\x -> x * x + x) 1
  , testCase "diffF" $ expect (list eq) [4, 1] $
      AD.diffF (\x -> [x * x, x + 1]) 2
  , testCase "diffF'" $ expect (list (pair eq)) [(4, 4), (3, 1)] $
      AD.diffF' (\x -> [x * x, x + 1]) 2
  , testCase "gradWith" $ expect (list (pair eq)) [(1, 2), (2, 1)] $
      D.gradWith (,) (\[x, y] -> x * y) [1, 2]
  , testCase "gradWith'" $ do
      let (value, grads) = D.gradWith' (,) (\[x, y] -> x * y) [1, 2]
      expect eq 2 value
      expect (list (pair eq)) [(1, 2), (2, 1)] grads
  , testCase "jacobian'" $ do
      let result = D.jacobian' g2 [1, 2]
      expect (list eq) [3, 2] (fmap fst result)
      expect (list (list eq)) [[1, 1], [2, 1]] (fmap snd result)
  , testCase "jacobianWith" $ expect (list (list (pair eq))) [[(1, 1), (2, 1)], [(1, 2), (2, 1)]] $
      D.jacobianWith (,) g2 [1, 2]
  , testCase "jacobianT" $ expect (list (list eq)) [[1, 2], [1, 1]] $
      AD.jacobianT g2 [1, 2]
  , testCase "du" $ expect eq 2 $
      AD.du (\[x, y] -> x * y) [(1, 1), (2, 0)]
  , testCase "duF" $ expect (list eq) [1, 2] $
      AD.duF (\[x, y] -> [x + y, x * y]) [(1, 1), (2, 0)]
  ]

nanInfCoverage :: TestTree
nanInfCoverage = testGroup "nan-inf"
  [ testCase "kahn-log" $ expect eq inf $ K.diff log 0
  , testCase "sparse-log" $ expect (list eq) [inf] $ S.grad (\[x] -> log x) [0]
  , testCase "numeric-ad-log" $ expect eq inf $ AD.diff log 0
  , testCase "numeric-ad-double-log" $ expect eq inf $ ADD.diff log 0
  , testCase "kahn-sqrt" $ assertBool "kahn diff sqrt -1 is NaN" $ isNaN (K.diff sqrt (-1))
  , testCase "sparse-sqrt" $ assertBool "sparse diff sqrt -1 is NaN" $
      case S.grad (\[x] -> sqrt x) [-1] of
        [value] -> isNaN value
        _ -> False
  , testCase "numeric-ad-sqrt" $ assertBool "numeric-ad diff sqrt -1 is NaN" $ isNaN (AD.diff sqrt (-1))
  ]

propertyCoverage :: TestTree
propertyCoverage = testGroup "properties"
  [ QC.testProperty "diff-square" propDiffSquare
  , QC.testProperty "grad-linear" propGradLinear
  , QC.testProperty "jacobian-linear" propJacobianLinear
  ]

propDiffSquare :: QC.Property
propDiffSquare = QC.forAll (QC.choose (-10, 10)) $ \x ->
  approxEq (2 * x) (AD.diff (\t -> t * t) x)

propGradLinear :: QC.Property
propGradLinear = QC.forAll (QC.choose (-10, 10)) $ \x ->
  QC.forAll (QC.choose (-10, 10)) $ \y ->
    list approxEq [3, 2] (AD.grad (\[a, b] -> 3 * a + 2 * b) [x, y])

propJacobianLinear :: QC.Property
propJacobianLinear = QC.forAll (QC.choose (-10, 10)) $ \x ->
  QC.forAll (QC.choose (-10, 10)) $ \y ->
    list (list approxEq) [[1, 1], [1, -1]] (AD.jacobian (\[a, b] -> [a + b, a - b]) [x, y])

f2 :: Floating a => [a] -> a
f2 [x, y] = x * x + y
f2 _ = 0

g2 :: Floating a => [a] -> [a]
g2 [x, y] = [x + y, x * y]
g2 _ = []

eq :: Double -> Double -> Bool
eq expected actual
  | isNaN expected = isNaN actual
  | isInfinite expected = isInfinite actual && signum actual == signum expected
  | otherwise = abs (actual - expected) <= 1e-12 * abs expected

approxEq :: Double -> Double -> Bool
approxEq expected actual
  | isNaN expected = isNaN actual
  | isInfinite expected = isInfinite actual && signum actual == signum expected
  | otherwise = abs (actual - expected) <= 1e-10 * max 1 (abs expected)

pair :: (a -> a -> Bool) -> (a, a) -> (a, a) -> Bool
pair eq' (expected1, expected2) (actual1, actual2) = eq' expected1 actual1 && eq' expected2 actual2

list :: (a -> a -> Bool) -> [a] -> [a] -> Bool
list eq' expected actual = length expected == length actual && and (zipWith eq' expected actual)

expect :: HasCallStack => Show a => (a -> a -> Bool) -> a -> a -> Assertion
expect eq' expected actual = eq' expected actual @? "expected " ++ show expected ++ " but got " ++ show actual

inf :: Double
inf = 1 / 0
