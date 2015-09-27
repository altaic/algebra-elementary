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


-- -- | Pretty-prints an 'Expr'.
-- instance Show Expr where
--   show (Coeff c       ) = show (fromRational c :: Double)
--   show (Var v         ) = show v
--   show (FunId i (v:vs)) = show i ++ "(" ++ L.foldr (\vv acc -> acc ++ ", " ++ show vv) (show v) vs ++ ")"
--   show (FunId i []    ) = show i ++ "()"
--   show (Mult []       ) = "()"
--   show (Mult [e]      ) = "(" ++ show e ++ ")"
--   show (Mult (e:es)   ) = "(" ++ show e ++ "⋅" ++ show (Mult es) ++ ")"
--   show (Add []        ) = "{}"
--   show (Add [e]       ) = "{" ++ show e ++ "}"
--   show (Add (e:es)    ) = show e ++ " + " ++ show (Add es)
--   show (Exp b e       ) = show b ++ "^(" ++ show e ++ ")"
--   show (Log b e       ) = "log_[" ++ show b ++ "](" ++ show e ++ ")"
--   show (Error s       ) = "[Error: " ++ s ++ "]"
