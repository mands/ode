-----------------------------------------------------------------------------
--
-- Module      :  Process.Flatten.InitialValueGen
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Initial Values Interpreter
-- These functions implement an interptert for a subset of the Core AST that is executed at compile time and used
-- to evaluate the initial values in a model, acting as a form of parital evalaution where we can simply store doubles
-- for the init values instead
--
-----------------------------------------------------------------------------

module Process.Flatten.InitialValueGen (
initialValueGen
) where

import Control.Monad.Writer
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Foldable as DF
import qualified Data.Traversable as DT
import qualified Utils.OrdMap as OrdMap

import Utils.CommonImports
import qualified Subsystem.Units as U
import qualified Subsystem.Types as T
import qualified AST.Core as AC
import AST.Common as ACO
import AST.Module

-- Types ---------------------------------------------------------------------------------------------------------------
-- Same monad as renamer
type InitM = WriterT InitMap MExcept
-- type ConvM = Supply Id

type InitMap = Map.Map Id Double
type InitEnv = Map.Map Id (AC.Expr Id)

--data InitState = InitState  { initMap :: InitMap
--                            , env :: Map.Map Id (AC.Expr Id)
--
--                            } deriving (Show, Eq, Ord)
-- mkInitState = InitState Map.empty Map.empty

-- Do we need typeMap too?
-- type InitData = (InitMap, InitEnv)

-- Entry Point ---------------------------------------------------------------------------------------------------------
initialValueGen :: Module Id -> MExcept (Module Id, InitMap)
initialValueGen mod@(LitMod modData) = do

    (env', initMap) <- runWriterT initGenM
    trace' [MkSB initMap, MkSB env'] "Flatten - Calculated init vals" $ return ()
    return (mod, initMap)
  where
    initGenM :: InitM InitEnv
    initGenM = DF.foldlM initTop Map.empty (modExprMap modData)

-- convert the toplet - we ensure that only TopLets with exist at this point
initTop :: InitEnv -> AC.TopLet Id -> InitM InitEnv
initTop env (AC.TopLet isInit t (ids) cE) = do
    (cE', _) <- initIntExpr cE env
    when isInit $ do
        let (AC.Lit (AC.Num d U.NoUnit)) = cE'
        insertInit (head ids) d
    return $ updateEnv ids cE' env

initTop _ coreExpr = errorDump [MkSB coreExpr] "Cannot interpret top expression" assert


-- | Interpret a restricted subset of the Core AST
-- we auumsme App and Abs already handled, only local vars (modules inlined),
initIntExpr :: AC.Expr Id -> InitEnv -> InitM (AC.Expr Id, InitEnv)

-- Var
initIntExpr e@(AC.Var (AC.LocalVar v) Nothing) env = return (lookupVar v env, env)
initIntExpr e@(AC.Var (AC.LocalVar v) (Just recId)) env = return (nEs Map.! recId, env)
  where
    (AC.Record nEs) = lookupVar v env

-- Nested Let, eval e1, add to env and eval e2, return e2'
initIntExpr e@(AC.Let isInit t ids e1 e2) env = do
    (e1', _) <- initIntExpr e1 env
    when isInit $ do
        let (AC.Lit (AC.Num d U.NoUnit)) = e1'
        insertInit (head ids) d
    (e2', _) <- initIntExpr e2 $ updateEnv ids e1' env
    return (e2', env)


-- Literals
initIntExpr e@(AC.Lit (AC.Num n U.NoUnit)) env = return (e, env)
initIntExpr e@(AC.Lit (AC.Boolean b)) env = return (e, env)
initIntExpr e@(AC.Lit (AC.Unit)) env = return (e, env)
initIntExpr e@(AC.Lit (AC.Time)) env = return (e, env)

-- Ops
initIntExpr e@(AC.Op op (AC.Tuple es)) env = do
    (vs, _) <- unzip <$> mapM (\e -> initIntExpr e env) es
    return (runOp op vs, env)
-- single-input Op
initIntExpr e@(AC.Op op e1) env = do
    (v, _) <- initIntExpr e1 env
    return (runOp op [v], env)

-- If
initIntExpr e@(AC.If eB eT eF) env = do
    ((AC.Lit (AC.Boolean b)), _) <- initIntExpr eB env
    (e', _) <- if b
        then initIntExpr eT env
        else initIntExpr eF env
    return (e', env)

-- Tuple - unpack, eval, then repack
initIntExpr e@(AC.Tuple es) env = do
    (vs, _) <- unzip <$> mapM (\e -> initIntExpr e env) es
    return (AC.Tuple vs, env)

-- Record - unpack, eval, then repack
initIntExpr e@(AC.Record nEs) env = do
    nVs  <- fmap fst <$> DT.mapM (\e -> initIntExpr e env) nEs
    return (AC.Record nVs, env)


initIntExpr e env = errorDump [MkSB e, MkSB env] "Cannot interpret expression" assert

-- Helper Functions ----------------------------------------------------------------------------------------------------

updateEnv :: AC.BindList Id -> AC.Expr Id -> InitEnv -> InitEnv
updateEnv (id:[]) e env = Map.insert id e env
updateEnv ids (AC.Tuple es) env = foldl (\env (id, e) ->  Map.insert id e env) env (zip ids es)

lookupVar :: Id -> InitEnv -> (AC.Expr Id)
lookupVar id env = case (Map.lookup id env) of
                    Just v -> v
                    Nothing ->  errorDump [MkSB id, MkSB env] "Cannot find value in environment" assert

insertInit :: Id -> Double -> InitM ()
insertInit id v = tell $ Map.singleton id v

runOp :: AC.Op -> [AC.Expr Id] -> AC.Expr Id
runOp op es = case op of
    -- Basic Ops
    (ACO.BasicOp ACO.Add)   -> runOpFF_F (+) es
    (ACO.BasicOp ACO.Sub)   -> runOpFF_F (-) es
    (ACO.BasicOp ACO.Mul)   -> runOpFF_F (*) es
    (ACO.BasicOp ACO.Div)   -> runOpFF_F (/) es

    (AC.BasicOp ACO.LT)     -> runOpFF_B (<) es
    (AC.BasicOp ACO.LE)     -> runOpFF_B (<=) es
    (AC.BasicOp ACO.GT)     -> runOpFF_B (>) es
    (AC.BasicOp ACO.GE)     -> runOpFF_B (>=) es
    (AC.BasicOp ACO.EQ)     -> runOpFF_B (==) es
    (AC.BasicOp ACO.NEQ)    -> runOpFF_B (/=) es

    (AC.BasicOp ACO.And)    -> runOpBB_B (&&) es
    (AC.BasicOp ACO.Or)     -> runOpBB_B (||) es
    (AC.BasicOp ACO.Not)    -> runOpB_B  (not) es

    -- Math Ops
    (AC.MathOp ACO.Sin)     -> runOpF_F  (sin) es
    (AC.MathOp ACO.Cos)     -> runOpF_F  (cos) es
    (AC.MathOp ACO.Tan)     -> runOpF_F  (tan) es
    -- simOp (AC.MathOp AC.SinCos)   ((Num n1):[])   = Tuple (Num $ sin n1, Num $ cos n2)

    (AC.MathOp ACO.ASin)    -> runOpF_F  (asin) es
    (AC.MathOp ACO.ACos)    -> runOpF_F  (acos) es
    (AC.MathOp ACO.ATan)    -> runOpF_F  (atan) es
    (AC.MathOp ACO.ATan2)   -> runOpFF_F (atan2) es

    (AC.MathOp ACO.Exp)     -> runOpF_F  (exp) es
    --simOp (AC.MathOp AC.Exp2)   ((Num n1):[])  = Num (exp2 n1)
    --simOp (AC.MathOp AC.Exp10)   ((Num n1):[])  = Num (exp10 n1)
    --simOp (AC.MathOp AC.Pow10)  ((Num n1):[])  = Num (pow10 n1)

    (AC.MathOp ACO.Log)     -> runOpF_F  (log) es
    --simOp (AC.MathOp AC.Log2)   ((Num n1):[])  = Num (log2 n1)
    --simOp (AC.MathOp AC.Log10)   ((Num n1):[])  = Num (log10 n1)
    --simOp (AC.MathOp AC.LogB)  ((Num n1):[])  = Num (logb n1)

    (AC.MathOp ACO.Pow)     -> runOpFF_F (**) es
    (AC.MathOp ACO.Sqrt)    -> runOpF_F  (sqrt) es
    (AC.MathOp ACO.Cbrt)    -> runOpF_F  (** (1/3)) es

    (AC.MathOp ACO.Hypot)   -> runOpFF_F (\n1 n2 -> sqrt (n1^2 + n2^2)) es
    --simOp (AC.MathOp AC.ExpM1)  ((Num n1):[])  = Num (expm1 n1)
    --simOp (AC.MathOp AC.Log1P)  ((Num n1):[])  = Num (log1p n1)

    (AC.MathOp ACO.SinH)    -> runOpF_F (sinh) es
    (AC.MathOp ACO.CosH)    -> runOpF_F (cosh) es
    (AC.MathOp ACO.TanH)    -> runOpF_F (tanh) es
    (AC.MathOp ACO.ASinH)   -> runOpF_F (asinh) es
    (AC.MathOp ACO.ACosH)   -> runOpF_F (acosh) es
    (AC.MathOp ACO.ATanH)   -> runOpF_F (atanh) es

    --simOp (AC.MathOp AC.Erf)  ((Num n1):[])   = Num (erf n1)
    --simOp (AC.MathOp AC.ErfC) ((Num n1):[])   = Num (erfc n1)
    --simOp (AC.MathOp AC.LGamma) ((Num n1):[])   = Num (lgamma n1)
    --simOp (AC.MathOp AC.TGamma) ((Num n1):[])   = Num (tgamma n1)

    (AC.MathOp ACO.FAbs)    -> runOpF_F (abs) es
    (AC.MathOp ACO.Floor)   -> runOpF_F (fromIntegral . floor) es
    (AC.MathOp ACO.Ceil)    -> runOpF_F (fromIntegral . ceiling) es
    (AC.MathOp ACO.Round)   -> runOpF_F (fromIntegral . round) es

    _ -> errorDump [MkSB op] "Cannot interpret operation" assert
  where
    runOpFF_F op    ((AC.Lit (AC.Num n1 U.NoUnit)):(AC.Lit (AC.Num n2 U.NoUnit)):[])    = (AC.Lit (AC.Num (op n1 n2) U.NoUnit))
    runOpFF_B op    ((AC.Lit (AC.Num n1 U.NoUnit)):(AC.Lit (AC.Num n2 U.NoUnit)):[])    = (AC.Lit (AC.Boolean (op n1 n2)))
    runOpBB_B op    ((AC.Lit (AC.Boolean b1)):(AC.Lit (AC.Boolean b2)):[])              = (AC.Lit (AC.Boolean (op b1 b2)))
    runOpB_B op     ((AC.Lit (AC.Boolean b1)):[])                                       = (AC.Lit (AC.Boolean (op b1)))
    runOpF_F op     ((AC.Lit (AC.Num n1 U.NoUnit)):[])                                  = (AC.Lit (AC.Num (op n1) U.NoUnit))
