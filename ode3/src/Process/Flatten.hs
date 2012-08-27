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

module Process.Flatten (
flatten
) where

import Control.Category
import Data.Label
import Prelude hiding ((.), id)

import qualified Data.Map as Map
import qualified Data.Bimap as Bimap

import Utils.CommonImports
import Subsystem.SysState

import AST.Common
import AST.Module

import qualified Utils.OrdMap as OrdMap
import qualified AST.Core as AC
import qualified AST.CoreFlat as ACF
import qualified Subsystem.Units as U
import Process.Flatten.ConvertAST


flatten :: String -> SysExcept ()
flatten initMod = do
    -- lookup the refmod in repl filedata
    replFD <- getSysState vLocalFile
    mod@(RefMod modFullName True sigMap lModEnv) <- lift $  maybeToExcept (Map.lookup (ModName initMod) (fileModEnv replFD))
                                                            (printf "Cannot find module %s loaded to simulate" (show initMod))

    -- inline mods

    -- inline components

    -- convert unitsDimI

    -- convert to CoreFlat
    -- TODO - tmp/dummy module here to fool later stages
    gModEnv <- getSysState vModEnv
    tmpMod <- lift $ getModuleGlobal modFullName gModEnv
    trace' [MkSB tmpMod] "CoreFlat AST input" $ return ()
    coreFlatMod <- lift $ convertAST tmpMod
    trace' [MkSB coreFlatMod] "CoreFlat AST output" $ return ()



    return ()


-- Unit Conversion -----------------------------------------------------------------------------------------------------

-- TODO - where does this func go - is run after unitconversion, during ANF conversion?
-- this prob needs supply monad to create a tmp var
-- converts an expression from the restrictred CExpr format into the general Core Expression for code-gen
convertCoreExpr :: U.CExpr -> AC.Expr Id
convertCoreExpr (U.CExpr op e1 e2) = AC.Op (convertCoreOp op) $ AC.Tuple [convertCoreExpr e1, convertCoreExpr e2]
  where
    convertCoreOp U.CAdd = AC.BasicOp Add
    convertCoreOp U.CSub = AC.BasicOp Sub
    convertCoreOp U.CMul = AC.BasicOp Mul
    convertCoreOp U.CDiv = AC.BasicOp Div

convertCoreExpr (U.CNum n) = AC.Lit $ AC.Num n U.NoUnit
-- TODO - this is broken!
convertCoreExpr U.CFromId = AC.Var (AC.LocalVar 1) Nothing


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
        topExpr'@(AC.TopLet s b expr) = fmap (+ deltaId) topExpr
        topExpr'' = AC.TopLet s b (updateVars expr)

        -- change all refernces from ModVar to LocalVar using the new ids, use the arg Id Bimap to calc
        -- TODO - use a traversable?
        argIdBimap = modIdBimap argModData

        updateVars :: AC.Expr Id -> AC.Expr Id
        updateVars (AC.Var (AC.ModVar modId id) mRecId)
            | modId == argName = AC.Var (AC.LocalVar (argIdBimap Bimap.! id)) mRecId

        updateVars (AC.App vId@(AC.ModVar modId id) expr)
            | modId == argName = AC.App (AC.LocalVar (argIdBimap Bimap.! id)) (updateVars expr)
        updateVars (AC.App vId expr) = AC.App vId (updateVars expr)

        updateVars (AC.Abs v expr) = AC.Abs v (updateVars expr)
        updateVars (AC.Let s b e1 e2) = AC.Let s b (updateVars e1) (updateVars e2)
        updateVars (AC.Op op e) = AC.Op op (updateVars e)
        updateVars (AC.If eIf eT eF) = AC.If (updateVars eIf) (updateVars eT) (updateVars eF)
        updateVars (AC.Tuple es) = AC.Tuple $ map updateVars es
        updateVars expr = expr


-- update mod data - sigMap doesn't change
appendModuleData :: ModData -> ModData -> ModData
appendModuleData argModData baseModData = baseModData { modTMap = typeMap', modIdBimap = idBimap', modFreeId = freeId' }
  where
    deltaId = fromJust $ (modFreeId argModData)
    typeMap' = Map.union (modTMap argModData) (Map.mapKeys (+ deltaId) (modTMap baseModData)) -- update the base ids and merge
    idBimap' = Bimap.fromList . map (\(srcId, id) -> (srcId, id + deltaId)) . Bimap.toList . modIdBimap $ baseModData   -- update the base ids only
    freeId' = liftM2 (+) (modFreeId argModData) (modFreeId baseModData) -- sum of both numbers
