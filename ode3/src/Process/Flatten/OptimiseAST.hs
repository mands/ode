-----------------------------------------------------------------------------
--
-- Module      :  Process.Flatten.OptimiseAST
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Performs some simple simulation-specific optimisations on the Core-level AST by pattern-matching
--
-- currently includes
-- * short-circuiting of boolean operators
-- * expansion of pow() calls
-- * convert upow and uroot into libm/stdlib compatible calls
-----------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, PatternGuards #-}

module Process.Flatten.OptimiseAST (
optimiseCoreAST
) where

import Control.Monad.State
import Utils.CommonImports
import Utils.MonadSupply

import qualified Data.Foldable as DF
import qualified Data.Traversable as DT


import AST.Common as AC
import qualified AST.Core as ACR
import AST.Module
import qualified Subsystem.Units as U
import qualified Subsystem.Types as T
import qualified Subsystem.SysState as Sys

-- Monad and Helper Funcs -----------------------------------------------------------------------------------------------------

type OptM = SupplyT Id (StateT OptState MExcept)
data OptState = OptState { tMap :: TypeMap } deriving (Show)


-- Entry Point ---------------------------------------------------------------------------------------------------------
-- we pass SimParams direct as need access to variety of opt options
optimiseCoreAST :: Sys.SimParams -> Module Id -> MExcept (Module Id)
optimiseCoreAST Sys.SimParams{..} (LitMod modData@ModData{..}) = do
    -- run non-monad opts
    let exprMap' = opts modExprMap
    -- run monad-opts
    ((exprMap'', freeIds'), _) <- runStateT (runSupplyT (optsM exprMap') [modFreeId ..]) $ OptState modTMap
    -- return the updated module
    return $ LitMod $ (updateModData2 modData exprMap'') { modFreeId = head freeIds' }
  where
    -- TODO - need to create generalised handler for multiple optimisations
    opts :: ExprMap Id -> ExprMap Id
    opts exprMap = (if _optShortCircuit then fmap optSCETop exprMap else exprMap) |> fmap optLibSubTop

    optsM :: ExprMap Id -> OptM (ExprMap Id)
    optsM exprMap = if _mathModel == Sys.Fast && _optPowerExpan then DT.mapM optPowerTop exprMap else return modExprMap


-- Perform Short-Circuit Evaluation ------------------------------------------------------------------------------------

optSCETop :: ACR.TopLet Id -> ACR.TopLet Id
optSCETop (ACR.TopLet isInit t bs tE) = ACR.TopLet isInit t bs $ optSCEExpr tE

optSCEExpr :: ACR.Expr Id -> ACR.Expr Id
-- e1 AND e2 => if (e1) then e2 else False
optSCEExpr e@(ACR.Op (AC.BasicOp (AC.And)) (ACR.Tuple (e1:e2:[]))) = trace' [MkSB e, MkSB e'] "SC AND Expr" $ e'
  where
    e' = ACR.If (optSCEExpr e1) (optSCEExpr e2) (ACR.Lit $ ACR.Boolean False)

-- e1 OR e2 => if (e1) then True else (e2)
optSCEExpr e@(ACR.Op (AC.BasicOp (AC.Or)) (ACR.Tuple (e1:e2:[]))) = trace' [MkSB e, MkSB e'] "SC OR Expr" $ e'
  where
    e' = ACR.If (optSCEExpr e1) (ACR.Lit $ ACR.Boolean True) (optSCEExpr e2)
-- don't care about the rest, pass on to mapExprM
optSCEExpr e = ACR.mapExpr optSCEExpr e


-- Perform Library substitions -----------------------------------------------------------------------------------------
optLibSubTop :: ACR.TopLet Id -> ACR.TopLet Id
optLibSubTop (ACR.TopLet isInit t bs tE) = ACR.TopLet isInit t bs $ optLibSubExpr tE

optLibSubExpr :: ACR.Expr Id -> ACR.Expr Id
-- convert upow direct to pow (power opt will expand later anyway)
optLibSubExpr e@(ACR.Op (AC.OtherOp (AC.UPow n)) e1) = trace' [MkSB e, MkSB e'] "UPow Sub" $ e'
  where
    e' = ACR.Op (AC.MathOp (AC.Pow)) (ACR.Tuple (e1':e2:[]))
    e1' = optLibSubExpr e1
    e2 = ACR.Lit $ ACR.Num (fromInteger n) U.NoUnit

-- convert uroot to either - sqrt, cbrt or pow calls
optLibSubExpr e@(ACR.Op (AC.OtherOp (AC.URoot n)) e1) = trace' [MkSB e, MkSB e'] "URoot Sub" $ e'
  where
    e' = case n of
            0 -> errorDump [MkSB e] "Should never occur, unit-checker should have caught earlier" assert
            1 -> e1'
            2 -> ACR.Op (AC.MathOp (AC.Sqrt)) (ACR.Tuple (e1':[]))
            3 -> ACR.Op (AC.MathOp (AC.Cbrt)) (ACR.Tuple (e1':[]))
            _ -> ACR.Op (AC.MathOp (AC.Pow)) (ACR.Tuple (e1':e2:[]))
    e1' = optLibSubExpr e1
    e2 = ACR.Lit $ ACR.Num (1 / fromInteger n) U.NoUnit

-- don't care about the rest, pass on to mapExprM
optLibSubExpr e = ACR.mapExpr optLibSubExpr e


-- Perform Pow() Expansion ---------------------------------------------------------------------------------------------
-- TODO - expand UPow Op too
optPowerTop :: ACR.TopLet Id -> OptM (ACR.TopLet Id)
optPowerTop (ACR.TopLet isInit t bs tE) = do
    ACR.TopLet isInit t bs <$> optPowerExpr tE

optPowerExpr :: ACR.Expr Id -> OptM (ACR.Expr Id)
optPowerExpr (ACR.Let isInit t bs e1 e2) = do
    ACR.Let isInit t bs <$> optPowerExpr e1 <*> optPowerExpr e2

-- pow(x, +n) => expand to n multiplications of x, when 0 <= n <= 8
optPowerExpr e@(ACR.Op (AC.MathOp (AC.Pow)) (ACR.Tuple (e1:(ACR.Lit (ACR.Num n _)):[]))) | n >= 0 && n <= 8 && isWholeNumber n = do
    e1' <- optPowerExpr e1
    e' <- expandPow e1' n
    trace' [MkSB e, MkSB e'] "Pow Expansion" $ return e'

-- don't care about the rest, pass on to mapExprM
optPowerExpr e = ACR.mapExprM optPowerExpr e

-- | Expand a x^n depending on n
-- This is part handled by LLVM's SimplfyLibCalls, including cases
-- * pow(1.0, x) -> 1.0
-- * pow(2.0, x) -> exp2(x)
-- * pow(x, 0.0) -> 1.0
-- * pow(x, 0.5) -> sqrt
-- * pow(x, 1.0) -> x
-- * pow(x, 2.0) -> x*x
-- * pow(x, -1.0) -> 1.0/x
expandPow :: ACR.Expr Id -> Double -> OptM (ACR.Expr Id)
expandPow e 0 = return $ ACR.Lit (ACR.Num 1 U.NoUnit)
expandPow e 1 = return e
-- is n > 1 and a whole number?
-- then create a new let to hold the expr, and a subexpr that handles the pow expansion
expandPow e n  = do
    trace' [MkSB n] "Expanding Pow func" $ return ()
    id <- supply
    -- create an id to hold the val
    s@OptState{..} <- lift get
    -- get the type - tho it should (always?) be a TFloat
    t <- lift . lift $ T.calcTypeExpr tMap e
    return $ ACR.Let False t [id] e $ createMultExpr id (floor n) (eRef id)
  where
    createMultExpr id 1 mE = mE
    createMultExpr id acc mE = createMultExpr id (acc-1) $
        ACR.Op (AC.BasicOp (AC.Mul)) (ACR.Tuple [mE, eRef id])

    eRef id = ACR.Var (ACR.LocalVar id) Nothing

-- Other Opts - TODO ---------------------------------------------------------------------------------------------------
