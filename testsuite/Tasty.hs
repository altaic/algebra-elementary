{-# OPTIONS_GHC -fdefer-typed-holes #-}


module Main where

import Test.Tasty
import Test.Tasty.HUnit
-- import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Numeric.Algebra.Elementary as A


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, properties]

properties :: TestTree
properties = localOption (mkTimeout 500000) (testGroup "QuickCheck Properties" [ qcMathSimplifier ])

qcMathSimplifier :: TestTree
qcMathSimplifier = testGroup "Simplifier"
  [ QC.testProperty "Simplify is Canonical" $ \e -> A.simplify e == A.simplify (A.simplify e) ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" [utMathSimplifier, utMathOther]

algVars :: [A.Expr]
algVars = map (\c -> A.mkVar [c]) ['a'..'z']

utMathSimplifier :: TestTree
utMathSimplifier = testGroup "Simplifier" [
  testCase "Flatten Nested Mult"     $ A.simplify (A.Mult [algVars!!0, A.Mult [algVars!!1, A.Mult [algVars!!2, algVars!!3], algVars!!4], algVars!!5])
                                     @?= A.Mult [algVars!!0, algVars!!1, algVars!!2, algVars!!3, algVars!!4, algVars!!5],
  testCase "Combine Mult Coeff"      $ A.simplify (A.Mult [algVars!!0, A.Coeff 2, algVars!!1, A.Coeff 3])
                                     @?= A.Mult [A.Coeff 6, algVars!!0, algVars!!1],
  testCase "Eliminate Mult One"      $ A.simplify (A.Mult [algVars!!0, A.Coeff 1, algVars!!1])
                                     @?= A.Mult [algVars!!0, algVars!!1],
  testCase "Eliminate Mult Triv"     $ A.simplify (A.Mult [algVars!!0])
                                     @?= algVars!!0,
  testCase "Combine Like Mult Terms" $ A.simplify (A.Mult [algVars!!0, algVars!!0])
                                     @?= A.Exp (algVars!!0) (A.Coeff 2),
  testCase "Flatten Nested Add"      $ A.simplify (A.Add [algVars!!0, A.Add [algVars!!1, A.Add [algVars!!2, algVars!!3], algVars!!4], algVars!!5])
                                     @?= A.Add [algVars!!0, algVars!!1, algVars!!2, algVars!!3, algVars!!4, algVars!!5],
  testCase "Combine Add Coeff"       $ A.simplify (A.Add [algVars!!0, A.Coeff 2, algVars!!1, A.Coeff 3])
                                     @?= A.Add [A.Coeff 5, algVars!!0, algVars!!1],
  testCase "Eliminate Add Zero"      $ A.simplify (A.Add [algVars!!0, A.Coeff 0, algVars!!1])
                                     @?= A.Add [algVars!!0, algVars!!1],
  testCase "Eliminate Add Triv"      $ A.simplify (A.Add [algVars!!0])
                                     @?= algVars!!0,
  testCase "Combine Like Add Terms"  $ A.simplify (A.Add [algVars!!0, algVars!!0])
                                     @?= A.Mult [A.Coeff 2, algVars!!0],
  testCase "Combine Exp Coeff"       $ A.simplify (A.Exp (A.Coeff 2) (A.Exp (A.Coeff 2) (algVars!!13)))
                                     @?= A.Exp (A.Coeff 4) (algVars!!13),
  testCase "Eliminate Exp Log"       $ A.simplify (A.Exp (A.Coeff 2) (A.Log (A.Coeff 2) (algVars!!0)))
                                     @?= algVars!!0,
  testCase "Expand Log Exp"          $ A.simplify (A.Log (A.Coeff 2) (A.Exp (algVars!!0) (algVars!!1)))
                                     @?= A.Mult [algVars!!1, A.Log (A.Coeff 2) (algVars!!0)],
  testCase "Expand Log Mult"         $ A.simplify (A.Log (A.Coeff 2) (A.Mult [algVars!!0, algVars!!1]))
                                     @?= A.Add [A.Log (A.Coeff 2) (algVars!!0), A.Log (A.Coeff 2) (algVars!!1)],
  testCase "Empty Mult Error"        $ A.simplify (A.Exp (A.Coeff 2) (A.Mult []))
                                     @?= A.Exp (A.Coeff 2) (A.Error "Empty Mult!"),
  testCase "Empty Add Error"         $ A.simplify (A.Exp (A.Coeff 2) (A.Add []))
                                     @?= A.Exp (A.Coeff 2) (A.Error "Empty Add!") ]

utMathOther :: TestTree
utMathOther = testGroup "Other" [
  -- testCase "Variable Substitution"             $ A.subst ("a", A.Exp (A.Coeff 2) (algVars!!13)) (A.Exp (algVars!!12) (algVars!!0))
  --                                              @?= A.Exp (algVars!!12) (A.Exp (A.Coeff 2) (algVars!!13)),
  testCase "Error Checking (raw, fail)"        $ A.check (A.Exp (A.Coeff 2) (A.mkVar ""))
                                               @?= False,
  testCase "Error Checking (raw, pass)"        $ A.check (A.Exp (A.Coeff 2) (algVars!!23))
                                               @?= True,
  testCase "Error Checking (simplified, fail)" $ A.check (A.simplify (A.Exp (A.Coeff 2) (A.mkVar "")))
                                               @?= False,
  testCase "Error Checking (simplified, pass)" $ A.check (A.simplify (A.Exp (A.Coeff 2) (algVars!!23)))
                                               @?= True,
  testCase "Equality (pass)"                   $ A.Exp (A.Coeff 2) (algVars!!23) == A.Exp (A.Coeff 2) (algVars!!23)
                                               @?= True,
  testCase "Equality (fail-1)"                 $ A.Exp (A.Coeff 3) (algVars!!23) == A.Exp (A.Coeff 2) (algVars!!23)
                                               @?= False,
  testCase "Equality (fail-2)"                 $ A.Exp (A.Coeff 2) (A.Mult [algVars!!23, algVars!!24]) == A.Exp (A.Coeff 2) (algVars!!23)
                                               @?= False ]
