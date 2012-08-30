-----------------------------------------------------------------------------
--
-- Module      :  Core.Validator
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
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
import Utils.Utils
import qualified Utils.OrdMap as OrdMap
import qualified AST.Core as E
import qualified AST.Module as M

-- Process Entry -------------------------------------------------------------------------------------------------------
validate :: M.Module E.DesId -> MExcept (M.Module E.DesId)
validate mod@(M.LitMod _ modData) = M.LitMod    <$> createTopExprs (M.modExprList modData) (M.modExportSet modData)
                                                <*> validateModData modData

validate mod@(M.FunctorMod funArgs _ modData) = M.FunctorMod    <$> funArgs'
                                                                <*> createTopExprs (M.modExprList modData) (M.modExportSet modData)
                                                                <*> validateModData modData
  where
    funArgs' =  if listUniqs funArgKeys then pure funArgs
                else throwError ("(VL05) - Functor has arguments with the same name")
    funArgKeys = OrdMap.keys funArgs

validateModData modData = return $ modData { M.modExprList = [] }


-- Binding datatypes ---------------------------------------------------------------------------------------------------
-- data Metadata = Metadata { bindMap :: Map.Map E.DesId Bool }
data ValidState = ValidState { exprMap :: M.ExprMap E.DesId, curBinds :: Set.Set E.DesId, sValSet :: Set.Set E.DesId, inSVal :: Bool }
mkValidState = ValidState OrdMap.empty Set.empty Set.empty False

addBinding :: Bool -> ValidState -> E.DesId -> MExcept ValidState
addBinding isSVal st b = case Set.member b (curBinds st) of
    True -> throwError $ printf "(VL04) - Binding %s already exists at this scoping level" b
    False -> return $ st { curBinds = Set.insert b (curBinds st)
                        , sValSet = if isSVal then Set.insert b (sValSet st) else (sValSet st) }

checkSVal :: E.DesId -> ValidState -> MExcept ()
checkSVal v st = unless (Set.member v (sValSet st)) $ throwError $ printf "(VL07) Value %s must be an init value" (show v)

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
        validExpr expr (st { curBinds = Set.empty, inSVal = sv })
        addTopBinding sv st bs topExpr
    t st topExpr@(E.TopType tName) = addTopBinding False st [tName] topExpr

    addTopBinding sv st bs expr = do
        st' <- DF.foldlM (addBinding sv) st bs
        return $ st' { exprMap = OrdMap.insert bs expr (exprMap st') }


-- Core Exprs ----------------------------------------------------------------------------------------------------------
-- check several properties for expression tree, passes state down into exp, doesn't bother returning it for now
validExpr :: E.Expr E.DesId -> ValidState -> MExcept ValidState
validExpr (E.Var (E.LocalVar v) _) st = if (inSVal st) then checkSVal v st >> return st else return st

validExpr (E.App v e) st = validExpr e st

validExpr (E.Abs b e) st = validExpr e st

validExpr (E.Let s t bs e1 e2) st = do
    -- reset the curBinds and update the SVal state flag
    validExpr e1 (st { curBinds = Set.empty, inSVal = s })
    validExpr e2 =<< DF.foldlM (addBinding s) st bs

validExpr (E.Op op e) st = validExpr e st
validExpr (E.If eB eT eF) st = validExpr eB st >>= validExpr eT >>= validExpr eF

validExpr (E.Tuple []) st = throwError "(VL01) Empty tuple found"
validExpr (E.Tuple (e:[])) st = throwError "(VL02) Tuple with only one element found"

-- use a map as they cannot create any new bindings (can still throw errors)
validExpr (E.Tuple es) st = DF.mapM_ (\e -> validExpr e st) es >> return st
validExpr (E.Record nEs) st = DF.mapM_ (\e -> validExpr e st) nEs >> return st

-- add ode & rre checks
validExpr (E.Ode (E.LocalVar initRef) e) st = checkSVal initRef st >> validExpr e st
validExpr (E.Rre (E.LocalVar src) (E.LocalVar dest) _) st = checkSVal src st >>  checkSVal dest st >> return st

validExpr e st = return st
