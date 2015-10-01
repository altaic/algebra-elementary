{-# OPTIONS_GHC -fdefer-typed-holes #-}



----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Algebra.Elementary.Rewrite
-- Copyright   :  William Knop 2015
-- License     :  BSD3
--
-- Maintainer  :  william.knop.nospam@gmail.com
-- Portability :  portable
--
-- Provides functions to manipulate elementary algebraic expressions.
--
-- __/TODO:/__ Write exports.

module Numeric.Algebra.Elementary.Rewrite where

import qualified Data.List                      as L
import           Numeric.Algebra.Elementary.AST



-- -------------------------------------------------------------------------------------------------
-- * Algebraic Expression Manipulation
-- -------------------------------------------------------------------------------------------------

-- | A Rewriter takes an 'Expr' and returns a simplified 'Expr'.
type Rewriter = Expr -> Expr

-- | Applies simplification rewriters repeatedly until the expression remains the same. Note that it
-- therefore has the property:
--
-- prop> simplify e == simplify (simplify e)
--
-- >>> simplify (Mult [Coeff 5, mkVar "x", Coeff 2])
-- Mult [Coeff (10 % 1),Var (Id {name = "x", unique = <1>})]
--
-- __/TODO:/__ Badly designed rewriters may cause the expressions to oscillate, which will send this
-- into an infinite loop and likely cause a stack overflow. We should really cap the number of times
-- rewriter can run, or somehow detect cycles and abort.
simplify :: Rewriter
simplify e = if e == se then e else simplify se
  where se = applyRewriters allSimplifiers e

-- | Takes a list of 'Rewriter's and applies them to an 'Expr' in order.
--
-- >>> applyRewriters [rewriteMultCoeff] (Mult [Coeff 5, mkVar "x", Coeff 2])
-- Mult [Coeff (10 % 1),Var (Id {name = "x", unique = <1>})]
applyRewriters :: [Rewriter] -> Expr -> Expr
applyRewriters ss op =
  case op of
    Add es   -> applyFuncs ss $ Add $ map (applyRewriters ss) es
    Mult es  -> applyFuncs ss $ Mult $ map (applyRewriters ss) es
    Exp b e  -> applyFuncs ss $ Exp (applyRewriters ss b) (applyRewriters ss e)
    Log b e  -> applyFuncs ss $ Log (applyRewriters ss b) (applyRewriters ss e)
    Coeff e  -> applyFuncs ss $ Coeff e
    Var i    -> applyFuncs ss $ Var i
    App i vs -> applyFuncs ss $ App i vs
    Error e  -> applyFuncs ss $ Error e -- __/NOTE:/__ I can't presently think of a reason to apply a rewriter to an Error, but I'll leave it just in case.
  where
    applyFuncs = flip (L.foldl' (flip ($)))



-- -------------------------------------------------------------------------------------------------
-- * Individual Rewriters
-- -------------------------------------------------------------------------------------------------

-- | Convenient list of all simplifiers. Note that some rewriters may not actually simplify.
allSimplifiers :: [Rewriter]
allSimplifiers = [ rewriteMultEmpty
                 , rewriteAddEmpty
                 , rewriteMultFlatten
                 , rewriteAddFlatten
                 , rewriteMultAlike
                 , rewriteLogMult
                 , rewriteAddAlike
                 , rewriteMultCoeff
                 , rewriteMultOne
                 , rewriteAddCoeff
                 , rewriteAddZero
                 , rewriteLogExp
                 , rewriteLogBB
                 , rewriteExpLog
                 , rewriteExpCoeff
                 , rewriteExpOne
                 , rewriteMultTriv
                 , rewriteAddTriv
                 , rewriteSort
                 ]



-- -------------------------------------------------------------------------------------------------
-- *** Log Rewriters
-- -------------------------------------------------------------------------------------------------

-- | This applies the 'Log' 'Exp' simplification @log_a(b^c)) => c*log_a(b)@.
rewriteLogExp :: Rewriter
rewriteLogExp (Log b (Exp bb ee)) = Mult [ee, Log b bb]
rewriteLogExp e                   = e

-- | Expands addition in 'Log's, e.g. @log_2(a*b) => log_2(a) + log_2(b)@.
rewriteLogMult :: Rewriter
rewriteLogMult (Log b (Mult es)) = Add (map (Log b) es)
rewriteLogMult e                 = e

-- | Eliminates `Log` with the same base and expression, e.g. @log_b(b) => 1@.
rewriteLogBB :: Rewriter
rewriteLogBB (Log b e) | b == e = Coeff 1
rewriteLogBB e         = e



-- -------------------------------------------------------------------------------------------------
-- *** Exp Rewriters
-- -------------------------------------------------------------------------------------------------

-- | This applies the 'Exp' 'Log' simplification @a^(log_a(b)) => b@.
rewriteExpLog :: Rewriter
rewriteExpLog (Exp b (Log bb ee)) = if b == bb then ee else Exp b (Log bb ee)
rewriteExpLog e                   = e

-- | This applies the 'Exp' 'Coeff' simplification @2^2^n => 4^n@.
rewriteExpCoeff :: Rewriter
rewriteExpCoeff (Exp (Coeff a) (Coeff b))         = Coeff (toRational ((fromRational a ** fromRational b) :: Double))
rewriteExpCoeff (Exp (Coeff a) (Exp (Coeff b) e)) = Exp (Coeff (toRational ((fromRational a ** fromRational b) :: Double))) e
rewriteExpCoeff e                                 = e

-- | This applies the 'Exp' by one simplification @x^1 => x@.
rewriteExpOne :: Rewriter
rewriteExpOne (Exp e (Coeff 1)) = e
rewriteExpOne e                 = e



-- -------------------------------------------------------------------------------------------------
-- *** Mult Rewriters
-- -------------------------------------------------------------------------------------------------

-- | This applies the 'Mult' 'Coeff' simplification @2*x*3*y*4 => 24*x*y@.
rewriteMultCoeff :: Rewriter
rewriteMultCoeff (Mult es) = Mult ((Coeff c):ms) where (c, ms) = foldr (\e (cc, mms) -> case e of Coeff ccc -> (cc*ccc, mms); ee -> (cc, ee:mms)) (1, []) es
rewriteMultCoeff e         = e

-- | This applies the 'Mult' by one simplification @1*x => x@.
rewriteMultOne :: Rewriter
-- rewriteMultOne (Mult [e]) = e
rewriteMultOne (Mult (e:es)) = let f = filter (Coeff 1 /=) (e:es) in case f of [] -> Coeff 1; fs -> Mult fs
rewriteMultOne e             = e

-- | Drops 'Mult's with only one element.
rewriteMultTriv :: Rewriter
rewriteMultTriv (Mult [e]) = e
rewriteMultTriv e          = e

-- | Treats empty 'Mult' as 'Coeff' 1, in the convention of <https://en.wikipedia.org/wiki/Empty_product>.
rewriteMultEmpty :: Rewriter
rewriteMultEmpty (Mult []) = Coeff 1
rewriteMultEmpty e        = e

-- | Applies the common 'Mult' simplification @a*a*a => a^3@.
rewriteMultAlike :: Rewriter
rewriteMultAlike (Mult es) = Mult (foldr (\ees acc -> case ees of [eee] -> eee:acc; eee:eees -> (Exp eee (Coeff (fromIntegral (L.length eees)+1))):acc; [] -> acc) [] (L.group es))
rewriteMultAlike e         = e

-- | Flattens nested 'Mult's.
rewriteMultFlatten :: Rewriter
rewriteMultFlatten (Mult es) = Mult (foldr (\e acc -> case e of Mult ees -> ees ++ acc; ee -> ee:acc) [] es)
rewriteMultFlatten e         = e



-- -------------------------------------------------------------------------------------------------
-- *** Add Rewriters
-- -------------------------------------------------------------------------------------------------

-- | Applies the common 'Add' simplification @a+a+a => 3*a@.
rewriteAddAlike :: Rewriter
rewriteAddAlike (Add es) = Add (foldr (\ees acc -> case ees of [eee] -> eee:acc; eee:eees -> (Mult [Coeff (fromIntegral (L.length eees)+1), eee]):acc; [] -> acc) [] (L.group es))
rewriteAddAlike e        = e

-- | Drops 'Add's with only one element.
rewriteAddTriv :: Rewriter
rewriteAddTriv (Add [e]) = e
rewriteAddTriv e         = e

-- | Treats empty 'Add' as 'Coeff' 0, in the convention of <https://en.wikipedia.org/wiki/Empty_sum>.
rewriteAddEmpty :: Rewriter
rewriteAddEmpty (Add []) = Coeff 0
rewriteAddEmpty e        = e

-- | This applies the 'Add' zero simplification @x+0 => x@.
rewriteAddZero :: Rewriter
-- rewriteAddZero (Add [e])    = e
rewriteAddZero (Add (e:es)) = let f = filter (Coeff 0 /=) (e:es) in case f of [] -> Coeff 1; fs -> Add fs
rewriteAddZero e            = e

-- | Flattens nested 'Add's.
rewriteAddFlatten :: Rewriter
rewriteAddFlatten (Add es) = Add (foldr (\e acc -> case e of Add ees -> ees ++ acc; ee -> ee:acc) [] es)
rewriteAddFlatten e        = e

-- | This applies the 'Add' 'Coeff' simplification @2+x+3+y+4 => 9+x+y@.
rewriteAddCoeff :: Rewriter
rewriteAddCoeff (Add es) = Add ((Coeff c):ms) where (c, ms) = foldr (\e (cc, mms) -> case e of Coeff ccc -> (cc + ccc, mms); ee -> (cc, ee:mms)) (0, []) es
rewriteAddCoeff e        = e



-- -------------------------------------------------------------------------------------------------
-- *** Other Rewriters
-- -------------------------------------------------------------------------------------------------

-- | Sorts an 'Expr'.
rewriteSort :: Rewriter
rewriteSort (Add es)  = Add (L.sort es)
rewriteSort (Mult es) = Mult (L.sort es)
rewriteSort e         = e
