-----------------------------------------------------------------------------
--
-- Module      :  Core.Validator
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  BSD3
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | A basic validator for a Module expressions, perfromas very simple validation tasks
-- hopefully GADTs will remove a lot of these
--
-- Checks Performed
-- * Functor args names are different
-- * top-level and nested binding names are not duplicated
-- * Tuple errors - empty, only 1 elem
-- * Ode and RREs reference to init/state vals
-----------------------------------------------------------------------------

module Process.Validator (
validate,
) where

import Control.Monad.Error
import Control.Applicative
import Data.Functor
import Data.List (nub)
import qualified Data.Map as Map

import qualified Data.Traversable as DT
import qualified Data.Foldable as DF
import qualified Data.Set as Set
import Text.Printf (printf)
import Utils.CommonImports
import qualified Utils.OrdMap as OrdMap
import qualified AST.Core as E
import qualified AST.Module as M
import qualified AST.Common as AC


-- Process Entry -------------------------------------------------------------------------------------------------------
validate :: M.Module E.DesId -> MExcept (M.Module E.DesId)
validate mod@(M.LitMod modData) = M.LitMod    <$> validateModData modData

validate mod@(M.FunctorMod funArgs modData) = M.FunctorMod    <$> funArgs'
                                                              <*> validateModData modData
  where
    funArgs' =  do
        -- check all args are unique
        unless (listUniqs (OrdMap.keys funArgs)) $ throwError ("(VL05) - Functor has arguments with the same name")
        -- check all args don't clash with normally imported aliases
        unless (listUniqs $ (Map.keys $ M.modModEnv modData) ++ (OrdMap.keys funArgs)) $
            throwError ("(VL08) - Functor has arguments with same name as the alias of an imported module")
        return funArgs

validateModData modData = do
    -- check all import aliases are unique - this is actually checked elsewhere within modDef driver when adding a module to the modEnv
    unless (listUniqs $ (Map.keys $ M.modModEnv modData)) $ throwError ("(VL09) - Module has imports with the same alias")
    exprMap <- createTopExprs (M.modExprList modData) (M.modExportSet modData)
    return $ modData { M.modExprList = [], M.modExprMap = exprMap }


-- Binding datatypes ---------------------------------------------------------------------------------------------------
-- data Metadata = Metadata { bindMap :: Map.Map E.DesId Bool }
data ValidState = ValidState { exprMap :: M.ExprMap E.DesId, curBinds :: Set.Set E.DesId }
mkValidState = ValidState OrdMap.empty Set.empty

addBinding :: Bool -> ValidState -> E.DesId -> MExcept ValidState
addBinding isSVal st b = case Set.member b (curBinds st) of
    True -> throwError $ printf "(VL04) - Binding %s already exists at this scoping level" b
    False -> return $ st { curBinds = Set.insert b (curBinds st) }

-- Top Level Exprs -----------------------------------------------------------------------------------------------------
-- create the expression map and check for duplicated top-level bindings
createTopExprs :: M.ExprList -> Set.Set E.SrcId -> MExcept (M.ExprMap E.DesId)
createTopExprs exprList exports = do
    st' <- DF.foldlM t mkValidState exprList
    -- check all exports are within exprSet
    if exports `Set.isSubsetOf` (curBinds st')
        then return $ exprMap st'
        else throwError $ printf "(VL06) - References to unknown values found within export list"

  where
    -- folds over a set (that holds unique bindings) and the updated exprMap for each expr
    t :: ValidState -> E.TopLet E.DesId -> MExcept ValidState
    t st topExpr@(E.TopLet sv ty bs expr) = do
        -- reset the curBinds and update the SVal state flag
        validExpr expr (st { curBinds = Set.empty })
        addTopBinding sv st bs topExpr

    t st topExpr@(E.TopType tName) = addTopBinding False st [tName] topExpr

    addTopBinding sv st bs expr = do
        st' <- DF.foldlM (addBinding sv) st bs
        return $ st' { exprMap = OrdMap.insert bs expr (exprMap st') }


-- Core Exprs ----------------------------------------------------------------------------------------------------------
-- check several properties for expression tree, passes state down into exp, doesn't bother returning it for now
validExpr :: E.Expr E.DesId -> ValidState -> MExcept ValidState
validExpr (E.Var (E.LocalVar v) _) st = return st

validExpr (E.App v e) st = validExpr e st

validExpr (E.Abs b e) st = validExpr e st

validExpr (E.Let s t bs e1 e2) st = do
    -- reset the curBinds
    validExpr e1 (st { curBinds = Set.empty })
    validExpr e2 =<< DF.foldlM (addBinding s) st bs

validExpr (E.Op op e) st = validExpr e st
validExpr (E.If eB eT eF) st = validExpr eB st >>= validExpr eT >>= validExpr eF

validExpr (E.Tuple []) st = throwError "(VL01) Empty tuple found"
validExpr (E.Tuple (e:[])) st = throwError "(VL02) Tuple with only one element found"

-- use a map as they cannot create any new bindings (can still throw errors)
validExpr (E.Tuple es) st = DF.mapM_ (\e -> validExpr e st) es >> return st
validExpr (E.Record nEs) st = DF.mapM_ (\e -> validExpr e st) nEs >> return st

-- add ode & rre checks
validExpr (E.Ode (E.LocalVar initRef) eD) st = validExpr eD st
validExpr (E.Sde (E.LocalVar initRef) eW eD) st = validExpr eW st >> validExpr eD st
validExpr (E.Rre srcs dests eR) st = validExpr eR st -- mapM (mapSndM checkVarId) srcs >> mapM (mapSndM checkVarId) srcs >> validExpr eR st

--  where
--    checkVarId (E.LocalVar lv) = return ()
--    checkVarId v = errorDump [MkSB v] "Intra-module init vals for Simops not handled correctly!" assert

validExpr e st = return st
