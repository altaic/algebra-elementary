{-# OPTIONS_GHC -fdefer-typed-holes #-}



----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Algebra.Elementary.Pretty
-- Copyright   :  William Knop 2015
-- License     :  BSD3
--
-- Maintainer  :  william.knop.nospam@gmail.com
-- Portability :  portable
--
-- Pretty printer for elementary algebraic expressions.
--
-- __/TODO:/__ Everything.
--
-- __/TODO:/__ Write exports.

module Numeric.Algebra.Elementary.Pretty where

import           Numeric.Algebra.Elementary.AST
import qualified Text.Nicify                    as N



prettyPrint :: Expr -> String
prettyPrint e = N.nicify (show e)
