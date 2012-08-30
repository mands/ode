-----------------------------------------------------------------------------
--
-- Module      :  Lang.Core.Flatten
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | TODO - main functioalyity that takes a set of Core modules and flattens them into CoreFlat AST
-- involves
-- * inline all modules
-- * inline components
-- * convert all units & drop unneeded type-checking info (wraps/newtypes and units)
-- * convert to ANF and CoreFlat AST

-- Notes
-- use intmap to hold local ids
-- need create a new ADT to hold metadata
-----------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Process.Flatten (
flatten
) where

import Control.Category
import qualified Data.Label as L
import Prelude hiding ((.), id)


import qualified Data.Traversable as DT
import qualified Data.Map as Map
import qualified Data.Bimap as Bimap

import Control.Monad.State
import Utils.MonadSupply

import Utils.CommonImports
import Subsystem.SysState

import AST.Common
import AST.Module

import qualified Utils.OrdMap as OrdMap
import qualified AST.Core as AC
import qualified AST.CoreFlat as ACF
import qualified Subsystem.Units as U

import Process.Flatten.ConvertAST
import Process.Flatten.ConvertTypes
import Process.Flatten.InlineComps


flatten :: String -> SysExcept ()
flatten initMod = do
    -- lookup the refmod in repl filedata
    replFD <- getSysState vLocalFile
    mod@(RefMod modFullName True sigMap lModEnv) <- lift $  maybeToExcept (Map.lookup (ModName initMod) (fileModEnv replFD))
                                                            (printf "Cannot find module %s loaded to simulate" (show initMod))

    gModEnv <- getSysState vModEnv
    tmpMod <- lift $ getModuleGlobal modFullName gModEnv
    trace' [MkSB tmpMod] "CoreFlat AST input" $ return ()

    -- inline mods


    -- flatten all nested lets
--    mod1 <- lift $ flattenExprs tmpMod
    --trace' [MkSB mod1] "Flatten exprs output" $ return ()

    -- inline components
    mod2 <- lift $ inlineComps tmpMod
    trace' [MkSB mod2] "Inline Comps output" $ return ()

    -- TODO - expand tuples and recs

    -- convert units and types
    -- TODO - tmp/dummy module here to fool later stages
--    unitsState <- getSysState lUnitsState
--    mod3 <- lift $ convertTypes mod2 unitsState
--    trace' [MkSB mod3] "Convert units output" $ return ()

    -- convert to CoreFlat
--    coreFlatMod <- lift $ convertAST mod3
--    trace' [MkSB coreFlatMod] "CoreFlat AST output" $ return ()


    return ()




-- Functor Application Helper Funcs ------------------------------------------------------------------------------------
-- TODO - utilise this code later when we unfold all modules into a single block of code

-- actually evaluate the functor applciation, similar to evaluation of function application
applyFunctor :: Module Id -> LocalModEnv -> Module Id
applyFunctor fMod@(FunctorMod fArgs fExprMap fModData) modEnv = resMod
  where
    -- best way to do this, create an empty lit mod, and add to it all the data by folding over the modEnv
    baseMod = LitMod fExprMap fModData
    resMod = Map.foldrWithKey appendModule baseMod modEnv

-- adds one module into another, basically mappend
appendModule :: ModName -> Module Id -> Module Id -> Module Id
appendModule argName argMod@(LitMod argExprMap argModData) baseMod@(LitMod baseExprMap baseModData) = baseMod'
  where
    baseMod' = LitMod exprMap' modData'
    deltaId = fromJust $ (modFreeId argModData)
    modData' = appendModuleData argModData baseModData

    -- update the expressions ids
    exprMap' = OrdMap.union argExprMap (OrdMap.map updateExprs baseExprMap)
    updateExprs (topB, topExpr) = (fmap (+ deltaId) topB, topExpr'')
      where
        topExpr'@(AC.TopLet s t b expr) = fmap (+ deltaId) topExpr
        topExpr'' = AC.TopLet s t b (updateVars expr)

        -- change all refernces from ModVar to LocalVar using the new ids, use the arg Id Bimap to calc
        -- TODO - use a traversable?
        argIdMap = modIdMap argModData

        updateVars :: AC.Expr Id -> AC.Expr Id
        updateVars (AC.Var (AC.ModVar modId id) mRecId)
            | modId == argName = AC.Var (AC.LocalVar (argIdMap Map.! id)) mRecId

        updateVars (AC.App vId@(AC.ModVar modId id) expr)
            | modId == argName = AC.App (AC.LocalVar (argIdMap Map.! id)) (updateVars expr)
        updateVars (AC.App vId expr) = AC.App vId (updateVars expr)

        updateVars (AC.Abs v expr) = AC.Abs v (updateVars expr)
        updateVars (AC.Let s t b e1 e2) = AC.Let s t b (updateVars e1) (updateVars e2)
        updateVars (AC.Op op e) = AC.Op op (updateVars e)
        updateVars (AC.If eIf eT eF) = AC.If (updateVars eIf) (updateVars eT) (updateVars eF)
        updateVars (AC.Tuple es) = AC.Tuple $ map updateVars es
        updateVars expr = expr


-- update mod data - sigMap doesn't change
appendModuleData :: ModData -> ModData -> ModData
appendModuleData argModData baseModData = baseModData { modTMap = typeMap', modIdMap = idMap', modFreeId = freeId' }
  where
    deltaId = fromJust $ (modFreeId argModData)
    freeId' = liftM2 (+) (modFreeId argModData) (modFreeId baseModData) -- sum of both numbers
    typeMap' = Map.union (modTMap argModData) (Map.mapKeys (+ deltaId) (modTMap baseModData)) -- update the base ids and merge
    idMap' = Map.map (+ deltaId) $ modIdMap baseModData   -- update the base ids only
