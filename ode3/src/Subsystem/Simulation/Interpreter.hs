-----------------------------------------------------------------------------
--
-- Module      :  Subsystem.Simulation.Interpreter
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Not implemented - should take a CoreFlat AST and simulate it
--
-----------------------------------------------------------------------------

module Subsystem.Simulation.Interpreter (
interpret
) where




import Control.Category
import qualified Data.Label as L
import Prelude hiding ((.), id)

import qualified Data.Foldable as DF
import qualified Data.Traversable as DT
import qualified Data.Map as Map
import qualified Utils.OrdMap as OrdMap

import Control.Monad.State
import Utils.MonadSupply

import Utils.CommonImports
import qualified Subsystem.SysState as Sys

import AST.Common as AC
import AST.CoreFlat


type SimM = StateT SimState MExceptIO

data SimState = SimState    { simEnv :: Map.Map Id Var, stateEnv :: Map.Map Id Var, curTime :: Double, curPeriod :: Integer, simParams :: Sys.SimParams }
mkSimState = SimState Map.empty Map.empty 0 0

interpret :: Module -> Sys.SysExceptIO ()
interpret mod = do

    -- setup the default simulation state
    p <- Sys.getSysState Sys.lSimParams
    liftIO $ debugM "ode3.sim" $ "Starting Simulation"
    lift $ runStateT runSimulation $ mkSimState p
    liftIO $ debugM "ode3.sim" $ "Simulation Complete"
    return ()
  where
    runSimulation :: SimM ()
    runSimulation = do
        -- setup time
        p <- simParams <$> get
        modify (\st -> st { curTime = (L.get Sys.lStartTime p) } )
        -- simulate the initial data
        _ <- unless (OrdMap.null (initExprs mod)) (simulateExprMap (initExprs mod) >> return ())
        -- simulate the loop exprs over the time period
        -- switch the maps
        modify (\st -> st { stateEnv = simEnv st, simEnv = Map.empty } )
        unless (OrdMap.null (loopExprs mod)) $ mapM_ simulateLoop [(L.get Sys.lStartTime p) + (L.get Sys.lTimestep p),2 * (L.get Sys.lTimestep p)..(L.get Sys.lEndTime p)]

    -- wrapper function to configure the cur time
    simulateLoop :: Double -> SimM ()
    simulateLoop t = do
        -- set the time
        modify (\st -> st { curTime = t } )
        liftIO $ putStrLn $ printf "In sim loop, time = %.4f" t

        st <- get
        trace' [MkSB (simEnv st), MkSB (stateEnv st)] "Current sim and state envs" $ return ()

        _ <- simulateExprMap (loopExprs mod)
        return ()


-- TODO - is this correct?
simulateExprMap :: ExprMap -> SimM Expr
simulateExprMap exprMap = do
    _ <- DF.mapM_ simulateExpr' exprBody
    -- need to return the last val
    retV <- simulateExpr' exprRet
    return $ retV
  where
    exprBody    = if (OrdMap.size exprMap > 1) then init $ OrdMap.toList exprMap else []
    exprRet     = last $ OrdMap.toList exprMap

    simulateExpr' :: (Id, ExprData) -> SimM Expr
    simulateExpr' (i, eD) = do
        e' <- simulateExpr eD
        -- store the val in the map -- which map!?
        modify (\st -> st { simEnv = Map.insert i (Num 3) (simEnv st) })
        return e'


simulateExpr :: ExprData -> SimM Expr

simulateExpr (ExprData (Var v) t) = Var <$> simulateVar v

-- process vs, then op
simulateExpr (ExprData (Op op vs) t) = do
    vs' <- mapM simulateVar vs
    return $ Var $ simulateOp op vs'

-- check the boolean and brach, we restart the entire simulateExprMap
simulateExpr (ExprData (If vB emT emF) t) = do
    (Boolean b) <- simulateVar vB
    if b
        then simulateExprMap emT
        else simulateExprMap emF

-- NOT YET IMPLEMENTED
--simulateExpr (i, (ExprData (Tuple vs) t)) = Tuple <$> mapM simulateVar vs

-- solve using a forward-Euler
simulateExpr (ExprData (Ode initId v) t) = do
    d@(Num dN) <- simulateVar v
    -- lookup initId in map
    n@(Num initN) <- (Map.!) <$> (stateEnv <$> get) <*> pure initId
    -- calc the ode
    p <- simParams <$> get
    let n' = Num $ initN + dN * (L.get Sys.lTimestep p)
    -- update the stateEnv
    modify (\st -> st { stateEnv = Map.insert initId n' (stateEnv st) })
    -- return the delta V
    return $ Var d


simulateVar :: Var -> SimM Var
-- lookup in env
simulateVar (VarRef i) = lookupId i
-- simulateVar (TupleRef i tupIdx) = undefined
-- lookup in env
simulateVar v@Time = Num <$> (curTime <$> get)
-- any literals just copy across
simulateVar v = return v


lookupId :: Id -> SimM Var
lookupId i = do
    mV <- Map.lookup <$> pure i <*> (simEnv <$> get)
    case mV of
        Just v  -> return v
        Nothing -> (Map.!) <$> (stateEnv <$> get) <*> pure i


-- | Takes an already evaulated list of vars and processes the builtin op
-- can pattern match these directly as we know the types are all correct
simulateOp :: AC.Op -> [Var] -> Var
-- Basic Ops
simulateOp (AC.BasicOp AC.Add)  ((Num n1):(Num n2):[])  = Num (n1 + n2)
simulateOp (AC.BasicOp AC.Sub)  ((Num n1):(Num n2):[])  = Num (n1 - n2)
simulateOp (AC.BasicOp AC.Mul)  ((Num n1):(Num n2):[])  = Num (n1 * n2)
simulateOp (AC.BasicOp AC.Div)  ((Num n1):(Num n2):[])  = Num (n1 / n2)

simulateOp (AC.BasicOp AC.LT)   ((Num n1):(Num n2):[])  = Boolean (n1 < n2)
simulateOp (AC.BasicOp AC.LE)   ((Num n1):(Num n2):[])  = Boolean (n1 <= n2)
simulateOp (AC.BasicOp AC.GT)   ((Num n1):(Num n2):[])  = Boolean (n1 > n2)
simulateOp (AC.BasicOp AC.GE)   ((Num n1):(Num n2):[])  = Boolean (n1 >= n2)
simulateOp (AC.BasicOp AC.EQ)   ((Num n1):(Num n2):[])  = Boolean (n1 == n2)
simulateOp (AC.BasicOp AC.NEQ)  ((Num n1):(Num n2):[])  = Boolean (n1 /= n2)

simulateOp (AC.BasicOp AC.And)  ((Boolean b1):(Boolean b2):[])  = Boolean (b1 && b2)
simulateOp (AC.BasicOp AC.Or)   ((Boolean b1):(Boolean b2):[])  = Boolean (b1 || b2)
simulateOp (AC.BasicOp AC.Not)  ((Boolean b1):[])               = Boolean (not b1)
-- Math Ops
simulateOp (AC.MathOp AC.Sin)   ((Num n1):[])   = Num (sin n1)
simulateOp (AC.MathOp AC.Cos)   ((Num n1):[])   = Num (cos n1)
simulateOp (AC.MathOp AC.Tan)   ((Num n1):[])   = Num (tan n1)

simulateOp (AC.MathOp AC.ASin)  ((Num n1):[])           = Num (asin n1)
simulateOp (AC.MathOp AC.ACos)  ((Num n1):[])           = Num (acos n1)
simulateOp (AC.MathOp AC.ATan)  ((Num n1):[])           = Num (atan n1)
simulateOp (AC.MathOp AC.ATan2) ((Num n1):(Num n2):[])  = Num (atan2 n1 n2)

simulateOp (AC.MathOp AC.Exp)   ((Num n1):[])   = Num (exp n1)
--simulateOp (AC.MathOp AC.Exp2)   ((Num n1):[])  = Num (exp2 n1)
--simulateOp (AC.MathOp AC.Exp10)   ((Num n1):[])  = Num (exp10 n1)
--simulateOp (AC.MathOp AC.Pow10)  ((Num n1):[])  = Num (pow10 n1)

simulateOp (AC.MathOp AC.Log)   ((Num n1):[])   = Num (log n1)
--simulateOp (AC.MathOp AC.Log2)   ((Num n1):[])  = Num (log2 n1)
--simulateOp (AC.MathOp AC.Log10)   ((Num n1):[])  = Num (log10 n1)
--simulateOp (AC.MathOp AC.LogB)  ((Num n1):[])  = Num (logb n1)

simulateOp (AC.MathOp AC.Pow)   ((Num n1):(Num n2):[])  = Num (n1 ** n2)
simulateOp (AC.MathOp AC.Sqrt)  ((Num n1):[])           = Num (sqrt n1)
simulateOp (AC.MathOp AC.Cbrt)  ((Num n1):[])           = Num (n1 ** (1/3))

simulateOp (AC.MathOp AC.Hypot) ((Num n1):(Num n2):[])  = Num (sqrt ( n1^2 + n2^2))
--simulateOp (AC.MathOp AC.ExpM1)  ((Num n1):[])  = Num (expm1 n1)
--simulateOp (AC.MathOp AC.Log1P)  ((Num n1):[])  = Num (log1p n1)

simulateOp (AC.MathOp AC.SinH)  ((Num n1):[])   = Num (sinh n1)
simulateOp (AC.MathOp AC.CosH)  ((Num n1):[])   = Num (cosh n1)
simulateOp (AC.MathOp AC.TanH)  ((Num n1):[])   = Num (tanh n1)
simulateOp (AC.MathOp AC.ASinH) ((Num n1):[])   = Num (asinh n1)
simulateOp (AC.MathOp AC.ACosH) ((Num n1):[])   = Num (acosh n1)
simulateOp (AC.MathOp AC.ATanH) ((Num n1):[])   = Num (atanh n1)

--simulateOp (AC.MathOp AC.Erf)  ((Num n1):[])   = Num (erf n1)
--simulateOp (AC.MathOp AC.ErfC) ((Num n1):[])   = Num (erfc n1)
--simulateOp (AC.MathOp AC.LGamma) ((Num n1):[])   = Num (lgamma n1)
--simulateOp (AC.MathOp AC.TGamma) ((Num n1):[])   = Num (tgamma n1)

-- NOT YET IMPLEMENTED
simulateOp op vs = errorDump [MkSB op, MkSB vs] "Operator not supported in interpreter" assert
















