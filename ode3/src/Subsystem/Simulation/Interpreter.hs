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
-- | Takes a CoreFlat AST and simulate is using an intenral interpreters with a Forward Euler
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
import Data.Fixed(mod')

import Control.Monad.State
import Utils.MonadSupply

import Utils.CommonImports
import qualified Subsystem.SysState as Sys

import AST.Common as AC
import AST.CoreFlat

import System.Random

type SimM = StateT SimState MExceptIO

data SimState = SimState    { _simEnv :: Map.Map Id Var, _stateEnv :: Map.Map Id Var
                            , _curTime :: Double, _curPeriod :: Integer, _outputHandle :: Handle
                            , _simParams :: Sys.SimParams, _rndGen :: StdGen
                            }

interpret :: Module -> Sys.SysExceptIO ()
interpret m@Module{..} = do
    -- setup the default simulation state
    p <- Sys.getSysState Sys.lSimParams
    liftIO $ debugM "ode3.sim" $ "Starting (Interpreted) Simulation"
    -- create the output file handle
    outHandle <- liftIO $ openBinaryFile (Sys._filename p) WriteMode
    -- write file header and initial data
    liftIO $ writeColumnHeader (Map.size $ initVals) [] outHandle
    liftIO $ writeRow (Sys._startTime p) (Map.elems $ Num <$> initVals) outHandle

    -- setup the initial data, wrap doubles into Var (Num d)
    randS <- liftIO $ newStdGen
    let initState = SimState Map.empty (Num <$> initVals) (Sys._startTime p) (Sys.calcOutputInterval p) outHandle p randS

    -- choose and start the correct the simulation model
    let simM =  case simType of
                    SimRRE  -> runSSA m p (Sys._startTime p) 1
                    -- ODE or SDE, simulate the loop exprs over the time period
                    _       -> unless (OrdMap.null loopExprs) $ runEulerMaruyama m p 1
    lift $ runStateT simM initState

    -- close the output file
    liftIO $ hFlush outHandle >> hClose outHandle
    liftIO $ debugM "ode3.sim" $ "(Interpreted) Simulation Complete"
    return ()
  where


-- Simulation Types ----------------------------------------------------------------------------------------------------
runEulerMaruyama :: Module -> Sys.SimParams -> Integer -> SimM ()
runEulerMaruyama m@Module{..} p curLoop = do
    runIter time -- run an iteration
    if time < (Sys.calcAdjustedStopTime p) then runEulerMaruyama m p (inc curLoop) else return () -- only loop again is time is less than endtime, break if equal/greater
  where
    time = (Sys._startTime p) + (fromInteger curLoop) * (Sys._timestep p)

    -- wrapper function to configure the cur time
    -- (equiv to simulate func in jitcompile/odelibrary)
    runIter :: Double -> SimM ()
    runIter t = do
        -- set the time
        modify (\st -> st { _curTime = t } )
        -- simulate the expr
        _ <- simExprMap loopExprs
        -- run the sim ops
        mapM_ simSimOp simOps
        -- reset sim envs
        modify (\st -> st { _simEnv = Map.empty })
        st <- get
        -- handle output period
        if ((_curPeriod st) == (Sys.calcOutputInterval $ _simParams st))
            then do
                -- write state env to disk
                liftIO $ writeRow t (Map.elems $ _stateEnv st) (_outputHandle st)
                modify (\st -> st { _curPeriod = 1 } )
            else
                modify (\st -> st { _curPeriod = inc $ _curPeriod st } )
        --trace' [MkSB t, MkSB (_simEnv st), MkSB (_stateEnv st)] "Current sim and state envs" $ return ()


runSSA :: Module -> Sys.SimParams -> Double -> Integer -> SimM ()
runSSA m@Module{..} p time curLoop = do
    -- run all exprs to setup the env (for dyn rate)
    -- set the sim env
    modify (\st -> st { _curTime = time, _simEnv = Map.empty } )
    -- simulate the expr
    _ <- simExprMap loopExprs

    sumProp <- sumPropensitities

    -- main reaction code
    if sumProp > 0 && time < (Sys._stopTime p)
        then do

            -- now run the SSA
            tau <- chooseTimestep sumProp
            r <- chooseReaction sumProp
            triggerReaction r
            let time' = time + tau

            -- output state
            curLoop' <- if time' >= (Sys._outputPeriod p * fromInteger curLoop)
                then do
                    st <- get
                    liftIO $ writeRow time' (Map.elems $ _stateEnv st) (_outputHandle st)
                    return $ inc curLoop
                else  return curLoop
            runSSA m p time' curLoop'
        else return ()
  where
    -- we know list of simops only contains RREs
    reactions = filter (\op -> case op of (Rre _ _ _) -> True; _ -> False) simOps

    chooseTimestep sumProp = do
        r1 <- getRandUniform
        return $ -(1/sumProp) * log(r1)

    chooseReaction sumProp = do
        r2 <- getRandUniform
        let endProp = r2 * sumProp
        (_, Just r) <- DF.foldlM (f endProp) (0, Nothing) reactions
        return r
      where
        f _ st@(curProp, Just r) _    =   return st
        f endProp st@(curProp, Nothing) r = do
            p <- calcPropensity r
            let curProp' = curProp + p
            if curProp' > endProp then return (curProp', Just r) else return (curProp', Nothing)

    triggerReaction (Rre srcs dests _) = mapM_ decPop srcs >> mapM_ incPop dests
      where
        decPop (_, v) = modify $ \st -> do
            let (Num n) = (_stateEnv st) Map.! v
            st { _stateEnv = Map.insert v (Num $ n - 1) (_stateEnv st) }
        incPop (stoc, v) = modify $ \st -> do
            let (Num n) = (_stateEnv st) Map.! v
            st { _stateEnv = Map.insert v (Num $ n + fromIntegral stoc) (_stateEnv st) }

    sumPropensitities :: SimM Double
    sumPropensitities = sum <$> mapM calcPropensity reactions

    -- does this not take into account the stoc of the srcs? no - only elementary reaction allowed
    calcPropensity (Rre srcs _ vR) = do
        s <- _stateEnv <$> get
        let srcPops = map (\(_, v) -> let (Num n) = s Map.! v in n) srcs
        -- get the calc expression rate
        (Num rate) <-  simVar vR
        return $ (product srcPops) * rate

-- Interpreter ---------------------------------------------------------------------------------------------------------

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
        modify (\st -> st { _simEnv = Map.insert i v' (_simEnv st) })
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
        env <- _simEnv <$> get
        v' <- simExprMap eM
        -- put the env back
        modify (\st -> st { _simEnv = env } )
        return v'


-- | Interpret a var operation
simVar :: Var -> SimM Var
-- refs lookup in env
simVar (VarRef i) = lookupId i
simVar (TupleRef i tupIdx) = do
    Tuple vs <- lookupId i
    return $ vs !! (fromInteger $ tupIdx - 1)
-- simple map over the vars
simVar (Tuple vs) = Tuple <$> mapM simVar vs
-- lookup in st env
simVar v@Time = Num <$> (_curTime <$> get)
-- calc a single/unique wiener - i.e. a normal-dist num, mean = 0, variance = sqrt(dt)
simVar v@Wiener = do
    dt <- Sys._timestep <$> _simParams <$> get
    randN <- getRandNormal
    return $ Num $ sqrt(dt)*randN

-- any other vars (will be literals - Num, Bool, Unit) are just copied across
simVar v = return v

lookupId :: Id -> SimM Var
lookupId i = do
    st <- get
    -- trace' [MkSB i, MkSB $ _simEnv st, MkSB $ _stateEnv st] "lookup id" $ return ()
    case Map.lookup i $ _simEnv st of
        Just v  -> return v
        Nothing -> return $ _stateEnv st Map.! i


-- | Takes an already evaulated list of vars and processes the builtin op
-- can pattern match these directly as we know the types are all correct
simOp :: AC.Op -> [Var] -> Var
-- Basic Ops
simOp (AC.BasicOp AC.Add)  ((Num n1):(Num n2):[])  = Num (n1 + n2)
simOp (AC.BasicOp AC.Sub)  ((Num n1):(Num n2):[])  = Num (n1 - n2)
simOp (AC.BasicOp AC.Mul)  ((Num n1):(Num n2):[])  = Num (n1 * n2)
simOp (AC.BasicOp AC.Div)  ((Num n1):(Num n2):[])  = Num (n1 / n2)
simOp (AC.BasicOp AC.Mod)  ((Num n1):(Num n2):[])  = Num (rem' n1 n2)

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

simOp (AC.MathOp AC.FAbs)   ((Num n1):[])   = Num (abs n1)
simOp (AC.MathOp AC.Floor)  ((Num n1):[])   = Num (fromIntegral $ floor n1)
simOp (AC.MathOp AC.Ceil)   ((Num n1):[])   = Num (fromIntegral $ ceiling n1)
simOp (AC.MathOp AC.Round)  ((Num n1):[])   = Num (fromIntegral $ round n1)

-- NOT YET IMPLEMENTED
simOp op vs = errorDump [MkSB op, MkSB vs] "Operator not supported in interpreter" assert

-- | Interpret a Simulation Operation - these are all stateful
-- solve an Ode using a forward-Euler
simSimOp (Ode initId v) = do
    -- trace' [] "entering solve ode" $ return ()
    st <- get
    -- trace' [MkSB $ _simEnv st, MkSB $ _stateEnv st] "envs" $ return ()
    -- pickup the delta via a VarRef into the loop state
    (Num dN) <- simVar v
    -- lookup initId in cur state map
    let (Num curN) = (_stateEnv st) Map.! initId
    -- calc the ode
    let dt = Sys._timestep $ _simParams st
    let n' = Num $ curN + dt * dN

    -- update the stateEnv -- we can do this destructively as the delta vars have already been calculated within the exprMap
    modify (\st -> st { _stateEnv = Map.insert initId n' (_stateEnv st) })
    -- trace' [MkSB initId, MkSB dN, MkSB curN, MkSB n'] "finished solved ode" $ return ()

-- solve a Sde using forward Euler-Maruyama
simSimOp (Sde initId vW vD) = do
    st <- get
    -- pickup the wiener via a VarRef into the loop state
    (Num dW) <- simVar vW
    -- pickup the delta via a VarRef into the loop state
    (Num dN) <- simVar vD
    -- lookup initId in cur state map
    let (Num curN) = (_stateEnv st) Map.! initId
    -- calc the sde
    let dt = Sys._timestep $ _simParams st
    randN <- getRandNormal
    let n' = Num $ curN + dt*dN + dW -- prev => dW*sqrt(dt)*randN
    -- update the stateEnv -- we can do this destructively as the delta vars have already been calculated within the exprMap
    modify (\st -> st { _stateEnv = Map.insert initId n' (_stateEnv st) })

-- Rre not supported in this sim model
simSimOp simOp@(Rre srcs dests rate) = errorDump [MkSB simOp] "RREs not supported in continous models" assert


-- File Output ---------------------------------------------------------------------------------------------------------

writeColumnHeader :: Int -> [String] -> Handle -> IO ()
writeColumnHeader n _ handle = do
    -- write Int/Word64 to BS
    let outBS = runPut $ putWord64le $ fromIntegral (n + 1) -- include time col
    -- TODO - write Column Headers to BS
    -- write BS to handle
    BL.hPut handle outBS

writeRow :: Double -> [Var] -> Handle -> IO ()
writeRow t vs handle = do
    -- trace' [MkSB t, MkSB vs] "writeRow" $ return ()
    -- convert Vars to Doubles and add time
    let ns = t : (filter (\v -> case v of Num n -> True; otherwise -> False) vs |> map (\(Num n) -> n))
    -- convert Doubles to Word64s and write to BS
    let outBS  = runPut $ mapM_ putFloat64le ns
    -- write BS to handle
    BL.hPut handle outBS
    -- hFlush handle
    return ()

-- Random Number Generation --------------------------------------------------------------------------------------------

-- helper functions to access random numbers
-- gets a uniform random number
getRandUniform :: SimM Double
getRandUniform = do
    s@SimState{_rndGen} <- get
    let (val, _rndGen') = random (_rndGen)
    put (s {_rndGen = _rndGen'})
    return val


-- returns a z-value based standard normal random number, based on the box-muller transform of uniform randoms
-- caching both values doesn't seem to work
getRandNormal :: SimM Double
getRandNormal = do
    snd <$> boxM
{-
    s <- get
    case (cacheNorm s) of
        Just y -> do
            put (s {cacheNorm = Nothing})
            return y
        Nothing -> do
            (x,y) <- boxM
            put (s {cacheNorm = Just y})
            return x
-}
  where
    -- apply the box-muller transform to obtain two guassiam-dist values, with 0 mean, and 1 varience
    boxM :: SimM (Double, Double)
    boxM = do
        u1 <- getRandUniform
        u2 <- getRandUniform
        let t1 = sqrt(-2 * log(u1))
        let t2 = 2*pi*u2
        return (t1*cos(t2), t1*sin(t2))
