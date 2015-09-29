{-# OPTIONS_GHC -fdefer-typed-holes #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  William Knop 2015
-- License     :  BSD3
--
-- Maintainer  :  william.knop.nospam@gmail.com
-- Portability :  portable
--
-- Provides functions to manipulate elementary algebraic expressions.
--
-- __/TODO:/__ Refactor and document.

module Main where

import qualified Data.Unique                        as U
import           Numeric.Algebra.Elementary.AST     as A
import           Numeric.Algebra.Elementary.Rewrite as R
import qualified System.IO.Unsafe                   as US
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck              as QC


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, properties]

properties :: TestTree
properties = localOption (mkTimeout 5000000) (testGroup "QuickCheck Properties" [ qcMathSimplifier ])

qcMathSimplifier :: TestTree
qcMathSimplifier = testGroup "Simplifier"
  [ QC.testProperty "Simplify Is Canonical" $ \e -> R.simplify e == R.simplify (R.simplify e) ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" [utMathSimplifier, utMathOther]

algVars :: [A.Expr]
algVars = map (\c -> A.mkVar [c]) ['a'..'z']

algSameIds :: [A.Id]
algSameIds = [A.mkId "a", A.mkId "a"]

algSameVars :: [A.Expr]
algSameVars = [A.mkVar "a", A.mkVar "a"]

uniques :: [U.Unique]
uniques = [US.unsafePerformIO U.newUnique, US.unsafePerformIO U.newUnique]

utMathSimplifier :: TestTree
utMathSimplifier = testGroup "Simplifier" [
  testCase "Flatten Nested Mult"     $ R.simplify (A.Mult [algVars!!0, A.Mult [algVars!!1, A.Mult [algVars!!2, algVars!!3], algVars!!4], algVars!!5])
                                     @?= A.Mult [algVars!!0, algVars!!1, algVars!!2, algVars!!3, algVars!!4, algVars!!5],
  testCase "Combine Mult Coeff"      $ R.simplify (A.Mult [algVars!!0, A.Coeff 2, algVars!!1, A.Coeff 3])
                                     @?= A.Mult [A.Coeff 6, algVars!!0, algVars!!1],
  testCase "Eliminate Mult One"      $ R.simplify (A.Mult [algVars!!0, A.Coeff 1, algVars!!1])
                                     @?= A.Mult [algVars!!0, algVars!!1],
  testCase "Eliminate Mult Triv"     $ R.simplify (A.Mult [algVars!!0])
                                     @?= algVars!!0,
  testCase "Combine Like Mult Terms" $ R.simplify (A.Mult [algVars!!0, algVars!!0])
                                     @?= A.Exp (algVars!!0) (A.Coeff 2),
  testCase "Flatten Nested Add"      $ R.simplify (A.Add [algVars!!0, A.Add [algVars!!1, A.Add [algVars!!2, algVars!!3], algVars!!4], algVars!!5])
                                     @?= A.Add [algVars!!0, algVars!!1, algVars!!2, algVars!!3, algVars!!4, algVars!!5],
  testCase "Combine Add Coeff"       $ R.simplify (A.Add [algVars!!0, A.Coeff 2, algVars!!1, A.Coeff 3])
                                     @?= A.Add [A.Coeff 5, algVars!!0, algVars!!1],
  testCase "Eliminate Add Zero"      $ R.simplify (A.Add [algVars!!0, A.Coeff 0, algVars!!1])
                                     @?= A.Add [algVars!!0, algVars!!1],
  testCase "Eliminate Add Triv"      $ R.simplify (A.Add [algVars!!0])
                                     @?= algVars!!0,
  testCase "Combine Like Add Terms"  $ R.simplify (A.Add [algVars!!0, algVars!!0])
                                     @?= A.Mult [A.Coeff 2, algVars!!0],
  testCase "Combine Exp Coeff"       $ R.simplify (A.Exp (A.Coeff 2) (A.Exp (A.Coeff 2) (algVars!!13)))
                                     @?= A.Exp (A.Coeff 4) (algVars!!13),
  testCase "Eliminate Exp One"       $ R.simplify (A.Exp (algVars!!13) (A.Coeff 1))
                                     @?= algVars!!13,
  testCase "Eliminate Exp Log"       $ R.simplify (A.Exp (A.Coeff 2) (A.Log (A.Coeff 2) (algVars!!0)))
                                     @?= algVars!!0,
  testCase "Expand Log Exp"          $ R.simplify (A.Log (A.Coeff 2) (A.Exp (algVars!!0) (algVars!!1)))
                                     @?= A.Mult [algVars!!1, A.Log (A.Coeff 2) (algVars!!0)],
  testCase "Expand Log Mult"         $ R.simplify (A.Log (A.Coeff 2) (A.Mult [algVars!!0, algVars!!1]))
                                     @?= A.Add [A.Log (A.Coeff 2) (algVars!!0), A.Log (A.Coeff 2) (algVars!!1)],
  testCase "Eliminate Log B B"       $ R.simplify (A.Log (algVars!!1) (algVars!!1))
                                     @?= Coeff 1,
  testCase "Simplify Empty Mult"     $ R.simplify (A.Exp (A.Coeff 2) (A.Mult []))
                                     @?= A.Coeff 2,
  testCase "Simplify Empty Add"      $ R.simplify (A.Exp (A.Coeff 2) (A.Add []))
                                     @?= A.Coeff 1 ]

utMathOther :: TestTree
utMathOther = testGroup "Other" [
  -- testCase "Variable Substitution"             $ A.subst ("a", A.Exp (A.Coeff 2) (algVars!!13)) (A.Exp (algVars!!12) (algVars!!0))
  --                                              @?= A.Exp (algVars!!12) (A.Exp (A.Coeff 2) (algVars!!13)),
  testCase "Error Checking (raw, fail)"        $ A.check (A.Exp (A.Coeff 2) (A.mkVar ""))
                                               @?= False,
  testCase "Error Checking (raw, pass)"        $ A.check (A.Exp (A.Coeff 2) (algVars!!23))
                                               @?= True,
  testCase "Error Checking (simplified, fail)" $ A.check (R.simplify (A.Exp (A.Coeff 2) (A.mkVar "")))
                                               @?= False,
  testCase "Error Checking (simplified, pass)" $ A.check (R.simplify (A.Exp (A.Coeff 2) (algVars!!23)))
                                               @?= True,
  testCase "Equality (pass)"                   $ A.Exp (A.Coeff 2) (algVars!!23)
                                               @?= A.Exp (A.Coeff 2) (algVars!!23),
  testCase "Equality (fail-1)"                 $ A.Exp (A.Coeff 3) (algVars!!23) == A.Exp (A.Coeff 2) (algVars!!23)
                                               @?= False,
  testCase "Equality (fail-2)"                 $ A.Exp (A.Coeff 2) (A.Mult [algVars!!23, algVars!!24]) == A.Exp (A.Coeff 2) (algVars!!23)
                                               @?= False ]
  -- testCase "Uniques Are Unique"                $ uniques!!0 /= uniques!!1
  --                                              @?= True,
  -- testCase "Ids Are Unique"                    $ algSameIds!!0 /= algSameIds!!1
  --                                              @?= True,
  -- testCase "Vars Are Unique"                   $ algSameVars!!0 /= algSameVars!!1
  --                                              @?= True ]
