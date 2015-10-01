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

import           Numeric.Algebra.Elementary.AST     as A
import           Numeric.Algebra.Elementary.Rewrite as R
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

utMathSimplifier :: TestTree
utMathSimplifier = testGroup "Simplifier" [
  testCase "Flatten Nested Mult"     $ R.simplify (A.Mult [A.mkVar "a", A.Mult [A.mkVar "b", A.Mult [A.mkVar "c", A.mkVar "d"], A.mkVar "e"], A.mkVar "f"])
                                     @?= A.Mult [A.mkVar "a", A.mkVar "b", A.mkVar "c", A.mkVar "d", A.mkVar "e", A.mkVar "f"],
  testCase "Combine Mult Coeff"      $ R.simplify (A.Mult [A.mkVar "a", A.Coeff 2, A.mkVar "b", A.Coeff 3])
                                     @?= A.Mult [A.Coeff 6, A.mkVar "a", A.mkVar "b"],
  testCase "Eliminate Mult One"      $ R.simplify (A.Mult [A.mkVar "a", A.Coeff 1, A.mkVar "b"])
                                     @?= A.Mult [A.mkVar "a", A.mkVar "b"],
  testCase "Eliminate Mult Triv"     $ R.simplify (A.Mult [A.mkVar "a"])
                                     @?= A.mkVar "a",
  testCase "Combine Like Mult Terms" $ R.simplify (A.Mult [A.mkVar "a", A.mkVar "a"])
                                     @?= A.Exp (A.mkVar "a") (A.Coeff 2),
  testCase "Flatten Nested Add"      $ R.simplify (A.Add [A.mkVar "a", A.Add [A.mkVar "b", A.Add [A.mkVar "c", A.mkVar "d"], A.mkVar "e"], A.mkVar "f"])
                                     @?= A.Add [A.mkVar "a", A.mkVar "b", A.mkVar "c", A.mkVar "d", A.mkVar "e", A.mkVar "f"],
  testCase "Combine Add Coeff"       $ R.simplify (A.Add [A.mkVar "a", A.Coeff 2, A.mkVar "b", A.Coeff 3])
                                     @?= A.Add [A.Coeff 5, A.mkVar "a", A.mkVar "b"],
  testCase "Eliminate Add Zero"      $ R.simplify (A.Add [A.mkVar "a", A.Coeff 0, A.mkVar "b"])
                                     @?= A.Add [A.mkVar "a", A.mkVar "b"],
  testCase "Eliminate Add Triv"      $ R.simplify (A.Add [A.mkVar "a"])
                                     @?= A.mkVar "a",
  testCase "Combine Like Add Terms"  $ R.simplify (A.Add [A.mkVar "a", A.mkVar "a"])
                                     @?= A.Mult [A.Coeff 2, A.mkVar "a"],
  testCase "Combine Exp Coeff"       $ R.simplify (A.Exp (A.Coeff 2) (A.Exp (A.Coeff 2) (A.mkVar "n")))
                                     @?= A.Exp (A.Coeff 4) (A.mkVar "n"),
  testCase "Eliminate Exp One"       $ R.simplify (A.Exp (A.mkVar "n") (A.Coeff 1))
                                     @?= A.mkVar "n",
  testCase "Eliminate Exp Log"       $ R.simplify (A.Exp (A.Coeff 2) (A.Log (A.Coeff 2) (A.mkVar "a")))
                                     @?= A.mkVar "a",
  testCase "Expand Log Exp"          $ R.simplify (A.Log (A.Coeff 2) (A.Exp (A.mkVar "a") (A.mkVar "b")))
                                     @?= A.Mult [A.mkVar "b", A.Log (A.Coeff 2) (A.mkVar "a")],
  testCase "Expand Log Mult"         $ R.simplify (A.Log (A.Coeff 2) (A.Mult [A.mkVar "a", A.mkVar "b"]))
                                     @?= A.Add [A.Log (A.Coeff 2) (A.mkVar "a"), A.Log (A.Coeff 2) (A.mkVar "b")],
  testCase "Eliminate Log B B"       $ R.simplify (A.Log (A.mkVar "b") (A.mkVar "b"))
                                     @?= Coeff 1,
  testCase "Simplify Empty Mult"     $ R.simplify (A.Exp (A.Coeff 2) (A.Mult []))
                                     @?= A.Coeff 2,
  testCase "Simplify Empty Add"      $ R.simplify (A.Exp (A.Coeff 2) (A.Add []))
                                     @?= A.Coeff 1 ]

utMathOther :: TestTree
utMathOther = testGroup "Other" [
  -- testCase "Variable Substitution"             $ A.subst ("a", A.Exp (A.Coeff 2) (A.mkVar "b"3)) (A.Exp (A.mkVar "b"2) (A.mkVar "a"))
  --                                              @?= A.Exp (A.mkVar "b"2) (A.Exp (A.Coeff 2) (A.mkVar "b"3)),
  testCase "Equality (pass)"                   $ A.Exp (A.Coeff 2) (A.mkVar "x")
                                               @?= A.Exp (A.Coeff 2) (A.mkVar "x"),
  testCase "Equality (fail-1)"                 $ A.Exp (A.Coeff 3) (A.mkVar "n") == A.Exp (A.Coeff 2) (A.mkVar "n")
                                               @?= False,
  testCase "Equality (fail-2)"                 $ A.Exp (A.Coeff 2) (A.Mult [A.mkVar "x", A.mkVar "y"]) == A.Exp (A.Coeff 2) (A.mkVar "x")
                                               @?= False ]
