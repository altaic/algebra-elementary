{-# OPTIONS_GHC -fdefer-typed-holes -fno-warn-orphans #-}
-- {-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}


----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Algebra.Elementary.AST
-- Copyright   :  William Knop 2015
-- License     :  BSD3
--
-- Maintainer  :  william.knop.nospam@gmail.com
-- Portability :  portable
--
-- Provides an AST for elementary algebraic expressions.
--
-- __/TODO:/__ It may be a good idea to generalize Expr such that Exp, Log, Sin, etc. are all some
-- sort of function to be applied. That way, one can add new functions with special
-- computational/simplifying behavior without having to mess with rewrite rules. I'm not entirely
-- clear on the benefits, but the generalization seems like one of those warm and fuzzy things. It
-- could also be evil.
--
-- __/TODO:/__ Add annotations. For instance, a weight annotation may be used for simplification via
-- an external solver.
--
-- __/TODO:/__ Write exports.
--
-- __/TODO:/__ Add trigonometric functions.
--
-- __/TODO:/__ Add summations and products. Can their representation somehow merge with 'Add' and
-- 'Mult'?

module Numeric.Algebra.Elementary.AST where

import           Control.Monad    (liftM, liftM2)
import           Data.Char        (ord)
import           Data.Set         (Set, empty, fromList, singleton, union, unions)
import           GHC.Generics     (Generic)
import qualified Test.QuickCheck  as QC



-- -------------------------------------------------------------------------------------------------
-- * Algebraic Expressions
-- -------------------------------------------------------------------------------------------------

-- | An algebraic expression, e.g. @2j^k + -4l^2 + -m + log_2(n)@, which ends up looking like this:
--
-- @
-- Add [
--   Mult [Coeff 2, Exp (Var (Id {name = "j", unique = \<1>})) (Var (Id {name = "k", unique = \<2>}))],
--   Mult [Coeff -4, Exp (Var (Id {name = "l", unique = \<3>})) (Coeff 2)],
--   Mult [Coeff -1, Var (Id {name = "m", unique = \<4>})],
--   Log (Coeff 2) (Var (Id {name = "n", unique = \<5>}))
--   ]
-- @
data Expr =
  Coeff Rational | -- ^ Coefficient, e.g. @2.2@
  Var Id         | -- ^ Variable, e.g. @x@
  App Id [Id]    | -- ^ Function Application, e.g. @f(x,y)@
  Mult [Expr]    | -- ^ Multiplication Operator, e.g. @a*b*c*...@
  Add [Expr]     | -- ^ Addition Operator, e.g. @a+b+c+...@
  Exp Expr Expr  | -- ^ Exponential Operator, e.g. @a^b@
  Log Expr Expr  | -- ^ Logarithmic Operator, e.g. @log_a(b)@
  Error String     -- ^ Error, e.g. @"Empty Mult is not okay!"@
  deriving (Eq, Show, Generic)

-- | Gives an ordering for 'Expr' for the purpose of sorting into a canonicalized form.
instance Ord Expr where
  compare e1 e2
    | e1 == e2  = EQ
    | otherwise = compare (rank e1) (rank e2)
    where
      rankId :: Id -> Double
      rankId (Id n) = sum $ map (fromIntegral . ord) n
      rankSt :: String -> Double
      rankSt s = sum $ map (fromIntegral . ord) s -- __/TODO/__: Possibly use Data.Char.digitToInt instead.
      rankHelper :: Double -> Double
      rankHelper 0 = 0
      rankHelper n = 1 - 1/n
      rank :: Expr -> Double
      rank (Coeff c)  = rankHelper (fromRational c)
      rank (Var i)    = 1 + rankHelper (rankId i)
      rank (App i vs) = 2 + rankHelper (rankId i) + rankHelper (sum $ map rankId vs)
      rank (Exp b e)  = 4 + rankHelper (rank b) + rankHelper (rank e)
      rank (Log b e)  = 6 + rankHelper (rank b) + rankHelper (rank e)
      rank (Add es)   = 8 + rankHelper (sum $ map rank es)
      rank (Mult es)  = 9 + rankHelper (product $ map rank es)
      rank (Error s)  = 10 + rankHelper (rankSt s)

-- | 'Arbitrary' 'Expr' instance for 'QuickCheck'.
instance QC.Arbitrary Expr where
  arbitrary = QC.sized arbExpr
    where
      arbExpr :: Int -> QC.Gen Expr
      arbExpr 0 = QC.oneof [ liftM Var (QC.arbitrary :: QC.Gen Id), liftM Coeff (QC.arbitrary :: QC.Gen Rational) ]
      arbExpr n = do
        j <- QC.choose (1,n)
        k <- QC.choose (1,3)
        QC.oneof [ liftM Var (QC.arbitrary :: QC.Gen Id)
                 , liftM Coeff QC.arbitrary
                 , liftM2 Exp (arbExpr j) (arbExpr (n-j))
                 , liftM2 Log (arbExpr j) (arbExpr (n-j))
                 , liftM Add (QC.vectorOf k (arbExpr (n `div` k)))
                 , liftM Mult (QC.vectorOf k (arbExpr (n `div` k)))
                 ]
  shrink = QC.genericShrink



-- -------------------------------------------------------------------------------------------------
-- * Algebraic Identifiers
-- -------------------------------------------------------------------------------------------------

-- | A unique algebraic identifier, e.g. @x@.
data Id = Id String
  deriving (Eq, Ord, Show, Generic)

-- | 'Arbitrary' 'Id' instance for 'QuickCheck'.
instance QC.Arbitrary Id where
  arbitrary = liftM Id (QC.choose (1,4) >>= genVarName)
    where
      genVarName :: Int -> QC.Gen String
      genVarName l = QC.vectorOf l $ QC.elements ['a'..'z']



-- -------------------------------------------------------------------------------------------------
-- * Algebraic Functions
-- -------------------------------------------------------------------------------------------------

-- | An algebraic function, e.g. @f(x,y) = x^2 + y^2@.
data Fun = Fun { ident :: Id, vars :: Set Id, expr :: Expr } deriving (Show, Generic)

-- | Equality according to unique function identifier.
instance Eq Fun where (==) Fun {ident=i1} Fun {ident=i2} = i1 == i2

-- | Order according to unique function identifier.
instance Ord Fun where compare Fun {ident=i1} Fun {ident=i2} = compare i1 i2



-- -------------------------------------------------------------------------------------------------
-- * Algebraic Universe
-- -------------------------------------------------------------------------------------------------

-- | This contains and tracks globally scoped stuff, e.g. functions.
--
-- __/TODO:/__ Necessary? Desirable?
data Universe = Universe { functions :: Set Fun }



-- -------------------------------------------------------------------------------------------------
-- * Utility Functions
-- -------------------------------------------------------------------------------------------------

-- instance Show Id where
--   show i = show name

-- | Makes an identifier.
--
-- >>> (mkId "x", mkId "y")
-- (Id {name = "x", unique = <1>},Id {name = "y", unique = <2>})
mkId :: String -> Id
mkId = Id

-- | Conveniently makes a variable for you.
--
-- >>> mkVar "x"
-- Var (Id {name = "x", unique = <1>})
mkVar :: String -> Expr
mkVar = Var <$> mkId

-- | Makes a unique algebraic function.
--
-- >>> mkFun "f" (Exp (mkVar "x") (Coeff 2))
-- Fun { ident = Id {name = "f", unique = <3>}
--     , vars = fromList [Id {name = "x", unique = <4>}]
--     , expr = Exp (Var (Id {name = "x", unique = <4>})) (Coeff (2 % 1))
--     }
mkFun :: String -> Expr -> Fun
mkFun n e = Fun { ident=mkId n, vars=getVars e, expr=e }
  where
    getVars :: Expr -> Set Id
    getVars (Var i)    = singleton i
    getVars (Add es)   = unions $ map getVars es
    getVars (Mult es)  = unions $ map getVars es
    getVars (Exp b ee) = getVars b `union` getVars ee
    getVars (Log b ee) = getVars b `union` getVars ee
    getVars (App _ vs) = fromList vs
    getVars _          = empty

-- -- | Substitutes an 'Expr' into an 'Expr', e.g. @a=2 /-> a + a^b^a => 2 + 2^b^2@.
-- subst :: Expr -> Expr -> Expr
-- subst b (Add es)     = Add (map (subst b) es)
-- subst b (Mult es)    = Mult (map (subst b) es)
-- subst b (Exp bb ee)  = Exp (subst b bb) (subst b ee)
-- subst b (Log bb ee)  = Log (subst b bb) (subst b ee)
-- subst (i, e) (Var v) = if (i == v) then e else Var v
-- subst _ e            = e



-- -- -------------------------------------------------------------------------------------------------
-- -- ** Annotations
-- -- -------------------------------------------------------------------------------------------------
--
-- -- | Individual annotations.
-- --
-- -- __/TODO:/__ Expand.
-- data AnnType = Weight Integer -- | Specifies that the annotated 'Expr' is given a weight for simplification.
--              | Other          -- | Placeholder.
--
-- data Annotation = Annotation { annotype    :: AnnType    -- | The annotation type.
--                              , expression  :: Expr     -- | The 'Expr' that's being annotated.
--                              , description :: String  -- | An optional descriptive string.
--                              }
--
-- annotateWeight :: Expr -> Annotation
-- annotateWeight e = case e of
--   (Coeff _) -> Annotation {annotype = Weight 1, expression=e, description=""}
--   (Var _)   -> Annotation {annotype = Weight 1, expression=e, description=""}
--   (Add _)   -> Annotation {annotype = Weight 1, expression=e, description=""}
--   (Mult _)  -> Annotation {annotype = Weight 1, expression=e, description=""}
--   (Exp _ _) -> Annotation {annotype = Weight 1, expression=e, description=""}
--   (Log _ _) -> Annotation {annotype = Weight 1, expression=e, description=""}
--   (App _ _) -> Annotation {annotype = Weight 1, expression=e, description=""}
--   (Error _) -> Annotation {annotype = Weight 1, expression=e, description=""}
--
-- totalWeight :: Annotation -> Integer
-- totalWeight Annotation { annotype=a, expression=e, description=d } = case a of
--   Weight w -> w + travExpr e
--   _        -> travExpr e
--   where
--     travExpr :: Expr -> Integer
--     travExpr (Coeff _) = 0
--     travExpr (Var _)   = 0
--     travExpr (App _ _) = 0
--     travExpr (Error _) = 0
--     travExpr (Add es)  = foldr (\e acc -> acc + totalWeight e) 0 es
--     travExpr (Mult es) = foldr (\e acc -> acc + totalWeight e) 0 es
--     travExpr (Exp b e) = (totalWeight b) + totalWeight e
--     travExpr (Log b e) = (totalWeight b) + totalWeight e


-- instance Eq Expr where
--   (==) (Coeff c1) (Coeff c2) = (c1 == c2)
--   (==) (Var v1) (Var v2) = (v1 == v2)

-- -- | Shows variable identifiers as characters.
-- instance Show Id where
--   show (Id i)
--     | i < 0   = "ERROR"
--     | i < 26  = [(L.genericIndex ['a'..'z'] i)]
--     | i >= 26 = replicate cnt (L.genericIndex ['a'..'z'] ind)
--     where
--       ind = mod i 26
--       cnt = fromIntegral $ div i 26
--   show (Id _) = undefined
