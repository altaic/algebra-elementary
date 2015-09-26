{-# OPTIONS_GHC -fdefer-typed-holes -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Algebra.Elementary
-- Copyright   :  William Knop 2015
-- License     :  BSD3
--
-- Maintainer  :  william.knop.nospam@gmail.com
-- Portability :  portable
--
-- Provides an AST and operators for elementary algebraic expressions.
--
-- __/TODO:/__ It may be a good idea to generalize Expr such that Exp, Log, Sin, etc. are all some
-- sort of function to be applied. That way, one can add new functions with special
-- computational/simplifying behavior without having to mess with rewrite rules. I'm not entirely
-- clear on the benefits, but the generalization seems like one of those warm and fuzzy things. It
-- could also be evil.
--
-- __/TODO:/__ Investigate numeric-prelude, subhask, mfsolve, Paraiso, etc.
--
-- __/TODO:/__ Add annotations. For instance, a weight annotation may be used for simplification via
-- an external solver.
--
-- __/TODO:/__ Split this into other modules. I expect there should be at least the following:
--
--   * AST
--   * Rewrite
--   * Compute
--   * Solve
--   * Pretty



module Numeric.Algebra.Elementary where

import           Control.Monad
import qualified Data.Char        as C
import qualified Data.List        as L
import qualified Data.Set         as S
import qualified Data.Unique      as U
import           System.IO.Unsafe
import qualified Test.QuickCheck  as QC



-- ------------------------------------------------------------
-- * Algebraic Universe
-- ------------------------------------------------------------

-- | This contains and tracks globally scoped stuff, e.g. functions.
--
-- __/TODO:/__ Necessary? Desirable?
data Universe = Universe { functions :: S.Set Fun }



-- ------------------------------------------------------------
-- * Algebraic Expressions
-- ------------------------------------------------------------

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
  deriving (Eq, Show)

-- | Gives an ordering for 'Expr' for the purpose of sorting into a canonicalized form.
instance Ord Expr where
  compare e1 e2
    | e1 == e2  = EQ
    | otherwise = compare (rank e1) (rank e2)
    where
      rankId :: Id -> Double
      rankId (Id {name=n}) = foldr (\c acc -> acc + fromIntegral (C.ord c)) 0 n
      rankSt :: String -> Double
      rankSt = foldr (\c acc -> acc + fromIntegral (C.ord c)) 0 -- __/TODO/__: Possibly use Data.Char.digitToInt instead.
      rankHelper :: Double -> Double
      rankHelper 0 = 0
      rankHelper n = 1 - 1/n
      rank :: Expr -> Double
      rank (Coeff c)  = rankHelper (fromRational c)
      rank (Var i)    = 1 + rankHelper (rankId i)
      rank (App i vs) = 2 + rankHelper (rankId i) + rankHelper (foldr (\v acc -> acc + rankId v) 0 vs)
      rank (Exp b e)  = 4 + rankHelper (rank b) + rankHelper (rank e)
      rank (Log b e)  = 6 + rankHelper (rank b) + rankHelper (rank e)
      rank (Add es)   = 8 + rankHelper (foldr (\e acc -> acc + rank e) 0 es)
      rank (Mult es)  = 9 + rankHelper (foldr (\e acc -> acc * rank e) 1 es)
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



-- ------------------------------------------------------------
-- * Algebraic Identifiers
-- ------------------------------------------------------------

-- | A unique algebraic identifier, e.g. @x@.
data Id = Id {name :: String, unique :: U.Unique }
  deriving (Eq, Show)

-- | Order identifiers first by name, then, if the names match, by unique.
instance Ord Id where
  compare Id {name=n1, unique=u1} Id {name=n2, unique=u2}
    | n1 /= n2  = compare n1 n2
    | otherwise = compare u1 u2

-- | Show uniques using their hash. Keep in mind that there can be collisions.
instance Show U.Unique where
  show u = "<" ++ show (U.hashUnique u) ++ ">"

-- | 'Arbitrary' 'Id' instance for 'QuickCheck'.
instance QC.Arbitrary Id where
  arbitrary = do
    n <- QC.choose (1,4) >>= genVarName
    return Id { name = n , unique = unsafePerformIO U.newUnique }
    where
      genVarName :: Int -> QC.Gen String
      genVarName l = QC.vectorOf l $ QC.elements ['a'..'z']

-- instance Show Id where
--   show i = show name

-- | Makes a unique identifier.
--
-- >>> mkId "x"
-- Id {name = "x", unique = <1>}
mkId :: String -> Id
mkId n = let u = unsafePerformIO U.newUnique in Id { name = n, unique = u }

-- | Conveniently makes a variable for you.
--
-- >>> mkVar "x"
-- Var (Id {name = "x", unique = <1>})
mkVar :: String -> Expr
mkVar = Var . mkId

-- ------------------------------------------------------------
-- * Algebraic Functions
-- ------------------------------------------------------------

-- | An algebraic function, e.g. @f(x,y) = x^2 + y^2@.
data Fun = Fun { ident :: Id, vars :: S.Set Id, expr :: Expr } deriving (Show)

-- | Equality according to unique function identifier.
instance Eq Fun where (==) Fun {ident=i1} Fun {ident=i2} = i1 == i2

-- | Order according to unique function identifier.
instance Ord Fun where compare Fun {ident=i1} Fun {ident=i2} = compare i1 i2

-- | Makes a unique algebraic function.
--
-- >>> mkFun "f" (Exp (mkVar "x") (Coeff 2))
-- Fun { ident = Id {name = "f", unique = <3>}
--     , vars = fromList [Id {name = "x", unique = <4>}]
--     , expr = Exp (Var (Id {name = "x", unique = <4>})) (Coeff (2 % 1))
--     }
mkFun :: String -> Expr -> Fun
mkFun n e = Fun { ident=mkId n, vars=getVars e, expr=e } where
  getVars :: Expr -> S.Set Id
  getVars (Var i)    = S.singleton i
  getVars (Add es)   = foldr (\ee acc-> S.union (getVars ee) acc) S.empty es
  getVars (Mult es)  = foldr (\ee acc-> S.union (getVars ee) acc) S.empty es
  getVars (Exp b ee)  = S.union (getVars b) (getVars ee)
  getVars (Log b ee)  = S.union (getVars b) (getVars ee)
  getVars (App _ vs) = S.fromList vs
  getVars _          = S.empty



-- ------------------------------------------------------------
-- * Algebraic Expression Manipulation
-- ------------------------------------------------------------

-- | A Simplifier takes an 'Expr' and returns a simplified 'Expr'.
type Simplifier = Expr -> Expr

-- | Squashes algebraic expressions in a single pass. Note that it therefore has the property:
--
-- prop> simplify e == simplify (simplify e)
--
-- __/FIXME:/__ The order of simplifiers may be wrong, else there's a bug in one of the simplifiers. The
-- simplifyLogMult doesn't seem to fire unless the simplifiers are all run twice.
--
-- >>> simplify (Mult [Coeff 5, mkVar "x", Coeff 2])
-- Mult [Coeff (10 % 1),Var (Id {name = "x", unique = <1>})]
simplify :: Simplifier
simplify = applySimplifiers allSimplifiers

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

-- | Verifies that an 'Expr' is valid or not.
--
-- __/TODO:/__ Is there a way to just prevent empty lists in the Expr types?
--
-- __/TODO:/__ Thread an exception through somehow for failure information?
--
-- >>> check (Mult [Coeff 5, mkVar "x", Coeff 2])
-- True
--
-- >>> check (Mult [Coeff 5, mkVar "", Coeff 2])
-- False
check :: Expr -> Bool
check (Add [])           = False
check (Add es)           = foldr (\e acc -> acc && check e) True es
check (Mult [])          = False
check (Mult es)          = foldr (\e acc -> acc && check e) True es
check (Log b e)          = check b && check e
check (Exp b e)          = check b && check e
check (Var Id {name=""}) = False
check (Error _)          = False
check _                  = True

-- -- | Substitutes an 'Expr' into an 'Expr', e.g. @a=2 /-> a + a^b^a => 2 + 2^b^2@.
-- subst :: Expr -> Expr -> Expr
-- subst b (Add es)     = Add (map (subst b) es)
-- subst b (Mult es)    = Mult (map (subst b) es)
-- subst b (Exp bb ee)  = Exp (subst b bb) (subst b ee)
-- subst b (Log bb ee)  = Log (subst b bb) (subst b ee)
-- subst (i, e) (Var v) = if (i == v) then e else Var v
-- subst _ e            = e



-- ------------------------------------------------------------
-- * Individual Rewriters (a.k.a. simplifiers)
-- ------------------------------------------------------------

-- | Convenient list of all simplifiers.
allSimplifiers :: [Simplifier]
allSimplifiers = [ simplifyError
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
                 , simplifyExpLog
                 , simplifyExpCoeff
                 , simplifyMultTriv
                 , simplifyAddTriv
                 , simplifySort
                 ]

-- | Empty 'Mult's, 'Add's, and 'Var's are invalid, so this nukes them.
simplifyError :: Simplifier
simplifyError (Mult [])          = Error "Empty Mult!"
simplifyError (Add [])           = Error "Empty Add!"
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

-- | Expands addition in 'Log's, e.g. @log_2(a*b) => log_2(a) + log_2(b)@.
simplifyLogMult :: Simplifier
simplifyLogMult (Log b (Mult es)) = Add (map (Log b) es)
simplifyLogMult e                 = e

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


-- -- ------------------------------------------------------------
-- -- ** Annotations
-- -- ------------------------------------------------------------
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
