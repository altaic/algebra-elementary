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

-- | A Simplifier takes an 'Expr' and returns a simplified 'Expr'.
type Simplifier = Expr -> Expr

-- | Squashes algebraic expressions in a single pass. Note that it therefore has the property:
--
-- prop> simplify e == simplify (simplify e)
--
-- __/FIXME:/__ The order of simplifiers may be wrong, else there's a bug in one of the simplifiers.
-- The simplifyLogMult doesn't seem to fire unless the simplifiers are all run multiple times. Need
-- to go through and figure out which simplifiers have dependencies on other ones.
--
-- >>> simplify (Mult [Coeff 5, mkVar "x", Coeff 2])
-- Mult [Coeff (10 % 1),Var (Id {name = "x", unique = <1>})]
simplify :: Simplifier
simplify e = applySimplifiers allSimplifiers (applySimplifiers allSimplifiers e)

-- | Takes a list of 'Simplifier's and applies them to an 'Expr' in order.
--
-- >>> applySimplifiers [simplifyMultCoeff] (Mult [Coeff 5, mkVar "x", Coeff 2])
-- Mult [Coeff (10 % 1),Var (Id {name = "x", unique = <1>})]
applySimplifiers :: [Simplifier] -> Expr -> Expr
applySimplifiers ss op =
  case op of
    Add es   -> applyFuncs ss $ Add $ map (applySimplifiers ss) es
    Mult es  -> applyFuncs ss $ Mult $ map (applySimplifiers ss) es
    Exp b e  -> applyFuncs ss $ Exp (applySimplifiers ss b) (applySimplifiers ss e)
    Log b e  -> applyFuncs ss $ Log (applySimplifiers ss b) (applySimplifiers ss e)
    Coeff e  -> applyFuncs ss $ Coeff e
    Var e    -> applyFuncs ss $ Var e
    App i vs -> applyFuncs ss $ App i vs
    Error e  -> applyFuncs ss $ Error e -- __/NOTE:/__ I can't presently think of a reason to apply a simplifier to an Error, but I'll leave it just in case.
  where
    applyFuncs = flip (L.foldl' (flip ($)))



-- -------------------------------------------------------------------------------------------------
-- * Individual Rewriters (a.k.a. simplifiers)
-- -------------------------------------------------------------------------------------------------

-- | Convenient list of all simplifiers.
allSimplifiers :: [Simplifier]
allSimplifiers = [ simplifyError
                 , simplifyMultEmpty
                 , simplifyAddEmpty
                 , simplifyMultFlatten
                 , simplifyAddFlatten
                 , simplifyMultAlike
                 , simplifyLogMult
                 , simplifyAddAlike
                 , simplifyMultCoeff
                 , simplifyMultOne
                 , simplifyAddCoeff
                 , simplifyAddZero
                 , simplifyLogExp
                 , simplifyLogBB
                 , simplifyExpLog
                 , simplifyExpCoeff
                 , simplifyExpOne
                 , simplifyMultTriv
                 , simplifyAddTriv
                 , simplifySort
                 ]

-- | Empty 'Mult's, 'Add's, and 'Var's are invalid, so this nukes them.
simplifyError :: Simplifier
simplifyError (Var Id {name=""}) = Error "Empty Var!"
simplifyError e                  = e

-- | This applies the 'Log' 'Exp' simplification @log_a(b^c)) => c*log_a(b)@.
simplifyLogExp :: Simplifier
simplifyLogExp (Log b (Exp bb ee)) = Mult [ee, Log b bb]
simplifyLogExp e                   = e

-- | This applies the 'Exp' 'Log' simplification @a^(log_a(b)) => b@.
simplifyExpLog :: Simplifier
simplifyExpLog (Exp b (Log bb ee)) = if b == bb then ee else Exp b (Log bb ee)
simplifyExpLog e                   = e

-- | This applies the 'Exp' 'Coeff' simplification @2^2^n => 4^n@.
simplifyExpCoeff :: Simplifier
simplifyExpCoeff (Exp (Coeff a) (Coeff b))         = Coeff (toRational ((fromRational a ** fromRational b) :: Double))
simplifyExpCoeff (Exp (Coeff a) (Exp (Coeff b) e)) = Exp (Coeff (toRational ((fromRational a ** fromRational b) :: Double))) e
simplifyExpCoeff e                                 = e

-- | This applies the 'Exp' by one simplification @x^1 => x@
simplifyExpOne :: Simplifier
simplifyExpOne (Exp e (Coeff 1)) = e
simplifyExpOne e                 = e

-- | Expands addition in 'Log's, e.g. @log_2(a*b) => log_2(a) + log_2(b)@.
simplifyLogMult :: Simplifier
simplifyLogMult (Log b (Mult es)) = Add (map (Log b) es)
simplifyLogMult e                 = e

-- | Eliminates `Log` with the same base and expression, e.g. @log_b(b) => 1@
simplifyLogBB :: Simplifier
simplifyLogBB (Log b e) | b == e = Coeff 1
simplifyLogBB e         = e

-- | This applies the 'Mult' 'Coeff' simplification @2*x*3*y*4 => 24*x*y@.
simplifyMultCoeff :: Simplifier
simplifyMultCoeff (Mult es) = Mult ((Coeff c):ms) where (c, ms) = foldr (\e (cc, mms) -> case e of Coeff ccc -> (cc*ccc, mms); ee -> (cc, ee:mms)) (1, []) es
simplifyMultCoeff e         = e

-- | This applies the 'Mult' by one simplification @1*x => x@.
simplifyMultOne :: Simplifier
-- simplifyMultOne (Mult [e]) = e
simplifyMultOne (Mult (e:es)) = let f = filter (Coeff 1 /=) (e:es) in case f of [] -> Coeff 1; fs -> Mult fs
simplifyMultOne e             = e

-- | Drops 'Mult's with only one element.
simplifyMultTriv :: Simplifier
simplifyMultTriv (Mult [e]) = e
simplifyMultTriv e          = e

-- | Treats empty 'Mult' as 'Coeff' 1, in the convention of <https://en.wikipedia.org/wiki/Empty_product>.
simplifyMultEmpty :: Simplifier
simplifyMultEmpty (Mult []) = Coeff 1
simplifyMultEmpty e        = e

-- | Applies the common 'Mult' simplification @a*a*a => a^3@
simplifyMultAlike :: Simplifier
simplifyMultAlike (Mult es) = Mult (foldr (\ees acc -> case ees of [eee] -> eee:acc; eee:eees -> (Exp eee (Coeff (fromIntegral (L.length eees)+1))):acc; [] -> acc) [] (L.group es))
simplifyMultAlike e         = e

-- | Applies the common 'Add' simplification @a+a+a => 3*a@
simplifyAddAlike :: Simplifier
simplifyAddAlike (Add es) = Add (foldr (\ees acc -> case ees of [eee] -> eee:acc; eee:eees -> (Mult [Coeff (fromIntegral (L.length eees)+1), eee]):acc; [] -> acc) [] (L.group es))
simplifyAddAlike e        = e

-- | Drops 'Add's with only one element.
simplifyAddTriv :: Simplifier
simplifyAddTriv (Add [e]) = e
simplifyAddTriv e         = e

-- | Treats empty 'Add' as 'Coeff' 0, in the convention of <https://en.wikipedia.org/wiki/Empty_sum>.
simplifyAddEmpty :: Simplifier
simplifyAddEmpty (Add []) = Coeff 0
simplifyAddEmpty e        = e

-- | Flattens nested 'Mult's.
simplifyMultFlatten :: Simplifier
simplifyMultFlatten (Mult es) = Mult (foldr (\e acc -> case e of Mult ees -> ees ++ acc; ee -> ee:acc) [] es)
simplifyMultFlatten e         = e

-- | This applies the 'Add' zero simplification @x+0 => x@.
simplifyAddZero :: Simplifier
-- simplifyAddZero (Add [e])    = e
simplifyAddZero (Add (e:es)) = let f = filter (Coeff 0 /=) (e:es) in case f of [] -> Coeff 1; fs -> Add fs
simplifyAddZero e            = e

-- | Flattens nested 'Add's.
simplifyAddFlatten :: Simplifier
simplifyAddFlatten (Add es) = Add (foldr (\e acc -> case e of Add ees -> ees ++ acc; ee -> ee:acc) [] es)
simplifyAddFlatten e        = e

-- | This applies the 'Add' 'Coeff' simplification @2+x+3+y+4 => 9+x+y@.
simplifyAddCoeff :: Simplifier
simplifyAddCoeff (Add es) = Add ((Coeff c):ms) where (c, ms) = foldr (\e (cc, mms) -> case e of Coeff ccc -> (cc + ccc, mms); ee -> (cc, ee:mms)) (0, []) es
simplifyAddCoeff e        = e

-- | Sorts an 'Expr'.
simplifySort :: Simplifier
simplifySort (Add es)  = Add (L.sort es)
simplifySort (Mult es) = Mult (L.sort es)
simplifySort e         = e
