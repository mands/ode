-----------------------------------------------------------------------------
--
-- Module      :  Process.Flatten.InlineMods
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Inline all modules, similar to inline comps
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, PatternGuards #-}

module Process.Flatten.InlineMods (
inlineMod
) where


import qualified Data.Traversable as DT
import qualified Data.Foldable as DF
import qualified Data.Map as Map

import Control.Monad.State
import Utils.MonadSupply


import Utils.CommonImports
import Subsystem.SysState
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

-- top-level entry point
inlineMod :: Module Id -> GlobalModEnv -> MExcept (Module Id)
inlineMod mod@(LitMod _) gModEnv = do
    (_ , st') <- runStateT (inlineMod' mod) (mkInlineState gModEnv)
    -- TODO - update the modData
    let (LitMod baseModData) = (stBaseMod st')
    return $ LitMod $ baseModData { modTMap = getTypesFromExpr (modExprMap baseModData) }


-- actuially handle inlining a module via inlining it's localenv
-- handles lits, functors, ref (which may have been partially-evaluated apps or vars)
-- we recursevly map over localModEnv of RefMods, inlining as required, and obtain a map of idmaps for updating the modvars refs as required
-- mod must be a Lit or Functor
inlineMod' :: Module Id -> InlineModsM ()
inlineMod' mod@(LitMod modData) = do
    -- have to convert to a list to mapM with keys
    modIdVarMap <- Map.fromList <$> (DT.mapM inlineLocalEnv . Map.toList $ modModEnv modData)
    -- patch up the refs, relocated and append the new module
    appendModule modIdVarMap mod


-- handle inlining a modules stored in localmodenv (i.e. always a refmod)
inlineLocalEnv :: (ModName, Module Id) -> InlineModsM (ModName, IdMap)
inlineLocalEnv (localModName, lMod@(RefMod modFullName True _ lModEnv)) | Map.size lModEnv == 0 = do
    inMods <- stInModMap <$> get
    case Map.lookup modFullName inMods of
        Just idMap  -> return (localModName, idMap)  -- module already loaded/inlined
        Nothing     -> do                            -- module not loaded/inlined
            -- load the module
            gModEnv <- stGModEnv <$> get
            inMod <- lift $ getModuleGlobal modFullName gModEnv
            -- recursively inline it
            -- TODO - add functor app code here
            inlineMod' inMod
            -- now get the relocated idMap
            idMap' <- (Map.!) <$> (stInModMap <$> get) <*> pure modFullName
            return (localModName, idMap')


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

        -- switch all vars eithin the expression to reference localvars within our inlined Mod
        updateExpr :: AC.Expr Id -> AC.Expr Id
        updateExpr (AC.Var mv@(AC.ModVar modId id) mRecId) | (Just idVarMap) <- Map.lookup modId modIdVarMap = AC.Var (AC.LocalVar (idVarMap Map.! id)) mRecId
        updateExpr (AC.App mv@(AC.ModVar modId id) e1) | (Just idVarMap) <- Map.lookup modId modIdVarMap = AC.App (AC.LocalVar (idVarMap Map.! id)) (updateExpr e1)
        updateExpr e = AC.mapExpr updateExpr e
