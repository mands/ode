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

-- Labels
import Control.Category
import qualified Data.Label as L
import Prelude hiding ((.), id)

-- File Output
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put
import Data.Binary.IEEE754
import System.IO

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

data SimState = SimState    { simEnv :: Map.Map Id Var, stateEnv :: Map.Map Id Var
                            , curTime :: Double, curPeriod :: Integer, outputHandle :: Handle
                            , simParams :: Sys.SimParams
                            }

mkSimState = SimState Map.empty Map.empty 0 0

interpret :: Module -> Sys.SysExceptIO ()
interpret mod = do
    -- setup the default simulation state
    p <- Sys.getSysState Sys.lSimParams
    liftIO $ debugM "ode3.sim" $ "Starting Simulation"
    -- create the output file handle
    outHandle <- liftIO $ openBinaryFile (L.get Sys.lFilename p) WriteMode
    -- write file header
    liftIO $ writeColumnHeader (OrdMap.size $ initExprs mod) [] outHandle
    -- run the simulation
    lift $ runStateT runSimulation $ mkSimState outHandle p
    -- close the output file
    liftIO $ hClose outHandle
    liftIO $ debugM "ode3.sim" $ "Simulation Complete"
    return ()
  where
    runSimulation :: SimM ()
    runSimulation = do
        p <- simParams <$> get
        -- simulate the initial data
        _ <- unless (OrdMap.null (initExprs mod)) (runSingleIteration (initExprs mod) (L.get Sys.lStartTime p))
        -- switch the maps
        modify (\st -> st { stateEnv = simEnv st, simEnv = Map.empty } )
        -- simulate the loop exprs over the time period
        unless (OrdMap.null (loopExprs mod)) $ mapM_ (runSingleIteration (loopExprs mod)) [(L.get Sys.lStartTime p) + (L.get Sys.lTimestep p)
                                                                        ,2 * (L.get Sys.lTimestep p)
                                                                                  ..(L.get Sys.lEndTime p)]

    -- wrapper function to configure the cur time
    runSingleIteration :: ExprMap -> Double -> SimM ()
    runSingleIteration eM t = do
        -- set the time
        modify (\st -> st { curTime = t, simEnv = Map.empty } )
        _ <- simExprMap eM
        st <- get
        trace' [MkSB t, MkSB (simEnv st), MkSB (stateEnv st)] "Current sim and state envs" $ return ()
        -- write cur init state
        liftIO $ writeRow (Map.elems $ stateEnv st) (outputHandle st)
        return ()

-- | Simulate an expression map using data in state monad
simExprMap :: ExprMap -> SimM Var
simExprMap exprMap = do
    _ <- DF.mapM_ simTopLet exprBody
    -- need to return the last val
    retV <- simTopLet exprRet
    return $ retV
  where
    -- split the exprMap into its body and final/return value
    exprBody    = if (OrdMap.size exprMap > 1) then init $ OrdMap.toList exprMap else []
    exprRet     = last $ OrdMap.toList exprMap

    -- | Interpret a single toplet, altering the env with the result var
    simTopLet :: (Id, ExprData) -> SimM Var
    simTopLet (i, eD) = do
        v' <- simExpr eD
        -- store the val in the map -- which map!?
        modify (\st -> st { simEnv = Map.insert i v' (simEnv st) })
        return v'


-- | Interpret a single expression
simExpr :: ExprData -> SimM Var
simExpr (ExprData (Var v) t) = simVar v

-- process vs, then op
simExpr (ExprData (Op op vs) t) = do
    vs' <- mapM simVar vs
    return $ simOp op vs'

-- check the boolean and brach, we restart the entire simExprMap
simExpr (ExprData (If vB emT emF) t) = do
    (Boolean b) <- simVar vB
    if b then simBranch emT else simBranch emF
  where
    simBranch eM = do
        -- save the cur simEnv
        env <- simEnv <$> get
        v' <- simExprMap eM
        -- put the env back
        modify (\st -> st { simEnv = env } )
        return v'

-- solve using a forward-Euler
simExpr (ExprData (Ode initId v) t) = do
    d@(Num dN) <- simVar v
    -- lookup initId in map
    n@(Num initN) <- (Map.!) <$> (stateEnv <$> get) <*> pure initId
    -- calc the ode
    p <- simParams <$> get
    let n' = Num $ initN + dN * (L.get Sys.lTimestep p)
    -- update the stateEnv
    modify (\st -> st { stateEnv = Map.insert initId n' (stateEnv st) })
    -- return the delta V
    return $ d


simVar :: Var -> SimM Var
-- refs lookup in env
simVar (VarRef i) = lookupId i
simVar (TupleRef i tupIdx) = do
    Tuple vs <- lookupId i
    return $ vs !! (fromInteger $ tupIdx - 1)
-- simple map over the vars
simVar (Tuple vs) = Tuple <$> mapM simVar vs
-- lookup in env
simVar v@Time = Num <$> (curTime <$> get)
-- any other vars (will be literals) are just copied across
simVar v = return v

lookupId :: Id -> SimM Var
lookupId i = do
    st <- get
    -- trace' [MkSB i, MkSB $ simEnv st, MkSB $ stateEnv st] "lookup id" $ return ()
    case Map.lookup i $ simEnv st of
        Just v  -> return v
        Nothing -> return $ stateEnv st Map.! i


-- | Takes an already evaulated list of vars and processes the builtin op
-- can pattern match these directly as we know the types are all correct
simOp :: AC.Op -> [Var] -> Var
-- Basic Ops
simOp (AC.BasicOp AC.Add)  ((Num n1):(Num n2):[])  = Num (n1 + n2)
simOp (AC.BasicOp AC.Sub)  ((Num n1):(Num n2):[])  = Num (n1 - n2)
simOp (AC.BasicOp AC.Mul)  ((Num n1):(Num n2):[])  = Num (n1 * n2)
simOp (AC.BasicOp AC.Div)  ((Num n1):(Num n2):[])  = Num (n1 / n2)

simOp (AC.BasicOp AC.LT)   ((Num n1):(Num n2):[])  = Boolean (n1 < n2)
simOp (AC.BasicOp AC.LE)   ((Num n1):(Num n2):[])  = Boolean (n1 <= n2)
simOp (AC.BasicOp AC.GT)   ((Num n1):(Num n2):[])  = Boolean (n1 > n2)
simOp (AC.BasicOp AC.GE)   ((Num n1):(Num n2):[])  = Boolean (n1 >= n2)
simOp (AC.BasicOp AC.EQ)   ((Num n1):(Num n2):[])  = Boolean (n1 == n2)
simOp (AC.BasicOp AC.NEQ)  ((Num n1):(Num n2):[])  = Boolean (n1 /= n2)

simOp (AC.BasicOp AC.And)  ((Boolean b1):(Boolean b2):[])  = Boolean (b1 && b2)
simOp (AC.BasicOp AC.Or)   ((Boolean b1):(Boolean b2):[])  = Boolean (b1 || b2)
simOp (AC.BasicOp AC.Not)  ((Boolean b1):[])               = Boolean (not b1)
-- Math Ops
simOp (AC.MathOp AC.Sin)   ((Num n1):[])   = Num (sin n1)
simOp (AC.MathOp AC.Cos)   ((Num n1):[])   = Num (cos n1)
simOp (AC.MathOp AC.Tan)   ((Num n1):[])   = Num (tan n1)
-- simOp (AC.MathOp AC.SinCos)   ((Num n1):[])   = Tuple (Num $ sin n1, Num $ cos n2)

simOp (AC.MathOp AC.ASin)  ((Num n1):[])           = Num (asin n1)
simOp (AC.MathOp AC.ACos)  ((Num n1):[])           = Num (acos n1)
simOp (AC.MathOp AC.ATan)  ((Num n1):[])           = Num (atan n1)
simOp (AC.MathOp AC.ATan2) ((Num n1):(Num n2):[])  = Num (atan2 n1 n2)

simOp (AC.MathOp AC.Exp)   ((Num n1):[])   = Num (exp n1)
--simOp (AC.MathOp AC.Exp2)   ((Num n1):[])  = Num (exp2 n1)
--simOp (AC.MathOp AC.Exp10)   ((Num n1):[])  = Num (exp10 n1)
--simOp (AC.MathOp AC.Pow10)  ((Num n1):[])  = Num (pow10 n1)

simOp (AC.MathOp AC.Log)   ((Num n1):[])   = Num (log n1)
--simOp (AC.MathOp AC.Log2)   ((Num n1):[])  = Num (log2 n1)
--simOp (AC.MathOp AC.Log10)   ((Num n1):[])  = Num (log10 n1)
--simOp (AC.MathOp AC.LogB)  ((Num n1):[])  = Num (logb n1)

simOp (AC.MathOp AC.Pow)   ((Num n1):(Num n2):[])  = Num (n1 ** n2)
simOp (AC.MathOp AC.Sqrt)  ((Num n1):[])           = Num (sqrt n1)
simOp (AC.MathOp AC.Cbrt)  ((Num n1):[])           = Num (n1 ** (1/3))

simOp (AC.MathOp AC.Hypot) ((Num n1):(Num n2):[])  = Num (sqrt ( n1^2 + n2^2))
--simOp (AC.MathOp AC.ExpM1)  ((Num n1):[])  = Num (expm1 n1)
--simOp (AC.MathOp AC.Log1P)  ((Num n1):[])  = Num (log1p n1)

simOp (AC.MathOp AC.SinH)  ((Num n1):[])   = Num (sinh n1)
simOp (AC.MathOp AC.CosH)  ((Num n1):[])   = Num (cosh n1)
simOp (AC.MathOp AC.TanH)  ((Num n1):[])   = Num (tanh n1)
simOp (AC.MathOp AC.ASinH) ((Num n1):[])   = Num (asinh n1)
simOp (AC.MathOp AC.ACosH) ((Num n1):[])   = Num (acosh n1)
simOp (AC.MathOp AC.ATanH) ((Num n1):[])   = Num (atanh n1)

--simOp (AC.MathOp AC.Erf)  ((Num n1):[])   = Num (erf n1)
--simOp (AC.MathOp AC.ErfC) ((Num n1):[])   = Num (erfc n1)
--simOp (AC.MathOp AC.LGamma) ((Num n1):[])   = Num (lgamma n1)
--simOp (AC.MathOp AC.TGamma) ((Num n1):[])   = Num (tgamma n1)

-- NOT YET IMPLEMENTED
simOp op vs = errorDump [MkSB op, MkSB vs] "Operator not supported in interpreter" assert


-- File Output ---------------------------------------------------------------------------------------------------------

writeColumnHeader :: Int -> [String] -> Handle -> IO ()
writeColumnHeader n _ handle = do
    -- write Int/Word32 to BS
    let outBS = runPut $ putWord32host $ fromIntegral n
    -- TODO - write Column Headers to BS
    -- write BS to handle
    BL.hPut handle outBS

writeRow :: [Var] -> Handle -> IO ()
writeRow vs handle = do
    -- convert Vars to Doubles
    let ns = filter (\v -> case v of Num n -> True; otherwise -> False) vs |> map (\(Num n) -> n)
    -- convert Doubles to Word64s and write to BS
    let outBS  = runPut $ mapM_ putFloat64le ns
    -- write BS to handle
    BL.hPut handle outBS
    return ()
