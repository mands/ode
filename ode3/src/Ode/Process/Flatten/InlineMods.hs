-----------------------------------------------------------------------------
--
-- Module      :  Process.Flatten.InlineMods
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  BSD3
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Inline all modules, similar to inline comps
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, PatternGuards #-}

module Ode.Process.Flatten.InlineMods (
inlineMod
) where


import qualified Data.Traversable as DT
import qualified Data.Foldable as DF
import qualified Data.Map as Map

import Control.Monad.State
import Ode.Utils.MonadSupply


import Ode.Utils.CommonImports
import Ode.Subsystem.SysState
import AST.Common
import AST.Module
import qualified AST.Core as AC
import qualified AST.CoreFlat as ACF
import qualified Subsystem.Units as U

import qualified Utils.OrdMap as OrdMap



-- State Data -----------------------------------------------------------------------------------------------------

type InlineModsM = StateT InlineState MExcept

-- holds the current map of inlined modules and their local ids
data InlineState = InlineState { stInModMap :: Map.Map ModFullName IdMap, stBaseMod :: Module Id, stGModEnv :: GlobalModEnv }
mkInlineState = InlineState Map.empty (LitMod mkModData)

-- Entry Point ---------------------------------------------------------------------------------------------------------
inlineMod :: GlobalModEnv -> Module Id -> MExcept (Module Id)
inlineMod gModEnv mod = do
    (_ , st') <- runStateT (inlineRefMod mod) (mkInlineState gModEnv)
    -- TODO - update the modData
    let (LitMod baseModData) = (stBaseMod st')
    return $ LitMod $ baseModData { modTMap = getTypesFromExpr (modExprMap baseModData) }


-- handle inlining a modules stored in a refmod (i.e. within a localmodenv)
inlineRefMod :: Module Id -> InlineModsM IdMap
inlineRefMod lMod@(RefMod modFullName isClosed _ lModEnv) = do
    unless isClosed $ throwError $ printf "%s is not a closed module, cannot inline" (show modFullName)
    -- get the inlinedMod state
    inMods <- stInModMap <$> get
    case Map.lookup modFullName inMods of
        Just idMap  -> return idMap  -- module already loaded/inlined
        Nothing     -> do            -- module not loaded/inlined
            -- load the module
            gModEnv <- stGModEnv <$> get
            inMod <- lift $ getModuleGlobal modFullName gModEnv
            -- recursively inline it - if we have a functor app, adjust the modEnv as required
            case Map.null lModEnv of
                True    -> inlineOrigMod inMod
                False   -> inlineOrigMod $ modifyModData
                    (\modData -> modData { modModEnv = (modModEnv modData) `Map.union` lModEnv }) inMod
            -- now get the relocated idMap
            idMap' <- (Map.!) <$> (stInModMap <$> get) <*> pure modFullName
            return idMap'


-- actually handle inlining a concrete module (Lit or Functor) via inlining it's localenv
-- handles lits, functors, ref (which may have been partially-evaluated apps or vars)
-- we recursevly map over localModEnv of RefMods, inlining as required, and obtain a map of idmaps for updating the modvars refs as required
inlineOrigMod :: Module Id -> InlineModsM ()
inlineOrigMod mod = do
    let modData = case getModData mod of
            Just mD -> mD
            Nothing -> errorDump [MkSB mod] "Trying to inline a module without modData" assert
    -- get the mapping from ids to their position within the baseMod have to convert to a list to mapM with keys
    modIdVarMap <- DT.mapM inlineRefMod $ modModEnv modData
    -- patch up the refs, relocated and append the new module
    appendModule modIdVarMap mod


-- insert the module to the stateMod, and relocate its vars so all bindings are still valid wrt the stateMod
appendModule :: Map.Map ModName IdMap -> Module Id -> InlineModsM ()
appendModule modIdVarMap srcMod = do
    baseMod <- stBaseMod <$> get
    -- relocate and append the src module
    let (baseMod', srcIdMap', srcModName) = appendModule' baseMod
    -- store the data
    modify (\st -> st { stBaseMod = baseMod', stInModMap = Map.insert srcModName srcIdMap' (stInModMap st) })
    return ()
  where
    -- convert/relocate the srcMod, and append it to the baseMod
    appendModule' baseMod@(LitMod baseModData) = (baseMod', srcIdMap', srcModName)
      where
        baseDeltaId = modFreeId baseModData
        baseFreeId' = baseDeltaId + modFreeId srcModData
        baseExprMap' = OrdMap.append (modExprMap baseModData) srcExprMap''
        baseMod' = LitMod $ baseModData { modExprMap = baseExprMap', modFreeId = baseFreeId' }

        srcModData = fromJust $ getModData srcMod
        srcModName = modFullName srcModData
        srcIdMap' = Map.map (+ baseDeltaId) $ modIdMap srcModData
        srcTMap' = Map.mapKeys (+ baseDeltaId) $ modTMap srcModData
        -- increment the mod freeIds by that contained in the baseMod
        srcExprMap' = OrdMap.map (\(bs, topE) -> (fmap (+baseDeltaId) bs, fmap (+baseDeltaId) topE)) (modExprMap srcModData)
        srcExprMap'' = updateModVars modIdVarMap srcExprMap'


    -- update all refs to modules utilising the new idmap
    updateModVars :: Map.Map ModName IdMap -> ExprMap Id -> ExprMap Id
    updateModVars modIdVarMap exprMap = fmap updateTop exprMap
      where
        updateTop :: AC.TopLet Id -> AC.TopLet Id
        updateTop (AC.TopLet b t bs e) = AC.TopLet b t bs $ updateExpr e
        updateTop (AC.TopType b) = AC.TopType b

        -- switch all vars eithin the expression to reference localvars within our inlined Mod
        updateExpr :: AC.Expr Id -> AC.Expr Id
        updateExpr (AC.Var mv mRecId) = AC.Var (updateIds mv) mRecId
        updateExpr (AC.App mv e1)  = AC.App (updateIds mv) (updateExpr e1)
        -- update sim ops
        updateExpr (AC.Ode mv e1) = AC.Ode (updateIds mv) (updateExpr e1)
        updateExpr (AC.Sde mv e1 e2) = AC.Sde (updateIds mv) (updateExpr e1) (updateExpr e2)
        updateExpr (AC.Rre srcs dests e1) = AC.Rre (map (mapSnd updateIds) srcs) (map (mapSnd updateIds) dests) (updateExpr e1)
        updateExpr (AC.Group mvs) = AC.Group $ map updateIds mvs
        updateExpr e = AC.mapExpr updateExpr e

        updateIds mv@(AC.ModVar modId id) | (Just idVarMap) <- Map.lookup modId modIdVarMap = AC.LocalVar (idVarMap Map.! id)
        updateIds v = v
