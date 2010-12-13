-----------------------------------------------------------------------------
--
-- Module      :  ModelSemantics
-- Copyright   :  Copyright (C) 2010 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | A range of functions taht perform checking on the model at a semantic level
--
-----------------------------------------------------------------------------

module ModelSemantics (
    modelCheck
) where

import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Foldable as F
import Control.Monad
import Control.Monad.Error
import Utilities
import AST
import ModelAST

-- should prob create a generic map function and make model a Functor

-- | check takes a model and runs a list of semantic checks on it to determine model validity
modelCheck :: Model -> MExcept Model
modelCheck = componentTypes >=> valueRefs >=> odesCheck
--modelCheck = componentTypes >=> odesCheck

-- | A basic check - do all the component calls match up to valid component types
componentTypes :: Model -> MExcept Model
componentTypes model@(Model sims components) =
    -- need pass over all components, check each component call to see if is correct
    case concatMap compCheck (Map.elems components) of
        list@(x:xs)  -> Left (unlines list)
        [] -> Right model
  where
    -- takes an component and checks all it's comp calls
    compCheck comp = mapMaybe compCallCheck (cBody comp)
      where
        -- checks an indiv comp call
        compCallCheck (CompCallDef outs name ins) =
            case (Map.lookup name components) of
                Nothing -> Just ("Unknown component " ++ name ++ " called from " ++ (cName comp))
                Just (Component _ inputs outputs _) -> if ((length ins == length inputs) && (length outs == length outputs))
                    then Nothing else Just ("Component " ++ name ++ " called from " ++ (cName comp) ++ " with wrong number of inputs/outputs")
        compCallCheck _ = Nothing

-- | TODO - checks that all value references are to values that exist in the current component scope
-- | Including for ode values, similar to typecheck code
-- | bummer, can't use mplus as it always chooses successful computation, doesn't propogate errors
-- | should switch from error to list monad, allowing all error possiblites to be evaluated non-deterministically
-- | need to build a list of ode/sde values first too
valueRefs :: Model -> MExcept Model
valueRefs model@(Model sims components) =
    mapM_ checkVals (Map.elems components) >> return model
  where
    -- checks an indiv component
    checkVals :: Component -> MExcept ()
    checkVals (Component n ins outs b) = do
        let ids = initialIds b ++ ins
        ids' <- F.foldlM checkCompStmts ids b
        mapM_ (checkExp ids') outs
        return ()

    -- checks each component stateemnt, i.e. value definition statements
    checkCompStmts :: [Id] -> CompStmt -> MExcept [Id]
    checkCompStmts s (ValueDef i e) = checkExp s e >> return (i:s)
    -- can use mapM or msum.map if needed
    checkCompStmts s (CompCallDef ids _ es) = mapM_ (checkExp s) es >> return (ids++s)

    -- checks indivudal expressions for value references
    checkExp :: [Id] -> Expr -> MExcept ()
    checkExp env (Number _) = return ()
    checkExp env (BinExpr x _ y) = (checkExp env x) >> (checkExp env y)

    checkExp env (FuncCall fname exps) = checkFuncs (length exps) >> mapM_ (checkExp env) exps
      where
        -- lets check the function exists and is called correctly
        checkFuncs 1 = lookupFunc uFuncs
        checkFuncs 2 = lookupFunc bFuncs
        checkFuncs _ = throwError "Functions of more than 2 args aren't supported"
        lookupFunc lst = if (elem fname (fst (unzip lst))) then return ()
            else throwError ("Function " ++ fname ++ " not found")

    checkExp env (ValueRef i) = if (elem i env)
        then return ()
        else throwError ("Reference to unknown value " ++ i ++ " found in component xxx!")
    checkExp env (CaseExp cs def) = checkExp env def >> mapM_ checkCases cs
      where
        checkCases (be, e) = (checkBoolExp env be) >> (checkExp env e)
    checkExp env (ODE _ _ e) = checkExp env e
    checkExp env (SDE _ _ e w) = checkExp env e >> checkExp env w


    -- checks boolean expressions
    checkBoolExp :: [Id] -> BoolExpr -> MExcept ()
    checkBoolExp env (LogExpr a _ b) = checkBoolExp env a >> checkBoolExp env b
    checkBoolExp env (RelExpr a _ b) = checkExp env a >> checkExp env b

initialIds :: [CompStmt] -> [Id]
initialIds comps = foldl initialDefs [] comps
  where
    initialDefs ids (ValueDef _ e) = initialVals ids e
    initialDefs ids (CompCallDef _ _ es) = foldl initialVals ids es
    initialVals ids (ODE i _ _) = i:ids
    initialVals ids (SDE i _ _ _) = i:ids
    initialVals ids _ = ids


-- | TODO - checks that all odes have a valid name (in parser?), are valid according to wrt and initial values
odesCheck :: Model -> MExcept Model
odesCheck model@(Model sims components) =
    Right model

-- TODO - Others...

