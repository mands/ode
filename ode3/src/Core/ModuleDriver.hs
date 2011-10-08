-----------------------------------------------------------------------------
--
-- Module      :  Core.ModuleDriver
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Runs the module system and interprets all module level commands
-- driver for the core language front-end of the compiler
-- will effectively run the front-end pipeline within the Error monad
-- requires calling reorderer, renamer, typechecker, converter/interpreter
-----------------------------------------------------------------------------

module Core.ModuleDriver (
moduleDriver,
) where


import Debug.Trace
import qualified Data.Foldable as DF
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Trans
import Control.Monad
import Control.Monad.Error

import System.Log.Logger

import qualified Core.AST as C
import Core.Reorderer (reorder)
import Core.Renamer (rename)
import Core.TypeChecker --(typeCheck, TypeVarEnv, TypeCons)
import Utils.Utils
import qualified Utils.OrdMap as OrdMap


-- | moduleDriver takes a list of base modules and creates a runtime module envinroment that is used create close modules
-- for simulation
moduleDriver :: [C.TopMod C.SrcId] -> IO (Maybe C.ModuleEnv)
moduleDriver baseModules = processRes
  where

    -- a rudimentary controlelr that sets up the iteration over the luist of module commands, altering state as we fold
    modEnv = DF.foldlM interpretModule Map.empty baseModules

    -- a top level IO command
    processRes = either
        (\err -> errorM "ode3.coreDriver" err >> return Nothing)
        (\res -> infoM "ode3.coreDriver" "No errors" >> infoM "ode3.coreDriver" (show res) >> return (Just res)) modEnv




-- a basic interpreter over the set of module types
interpretModule :: C.ModuleEnv -> C.TopMod C.SrcId -> MExcept C.ModuleEnv
interpretModule modEnv (C.TopMod name mod@(C.LitMod _ _)) = do
    -- reorder, rename and typecheck the expressinons within module, adding to the module metadata
    mod' <- reorder >=> rename >=> typeCheck $ mod
    return $ Map.insert name mod' modEnv

interpretModule modEnv (C.TopMod name mod@(C.FunctorMod _ _ _)) = do
    -- reorder, rename and typecheck the expressinons within functor module, adding to the module metadata
    mod' <- reorder >=> rename >=> typeCheck $ mod
    return $ Map.insert name mod' modEnv



interpretModule modEnv (C.TopMod name mod@(C.AppMod fModId argModIds)) = do

    -- need to check that the application is valid, if so then create a new module
    -- involves several steps with specialised pipeline operations

    -- reorder - place the expressions from args ahead of thos withing the func module
    -- renaming, use the free vars to deteermine a safe renaimg scheme
    -- typecheck, check the args are valid, then typecheck the signatures, using the same alogirthm as typechecking an app
    --  within an expression, run the same cosntain algorithm, then matchup the sigs

    -- order is, args/sig check, typecheck, rename, reorder
    -- should return a new closed module that can be reused later on
    --mod' <- reorder >=> rename >=> typeCheck $ mod
    --return $ Map.insert name mod' modEnv
    (fMod', appModEnv') <- typeCheckApp fMod appModEnv
    return $ trace (show fMod') modEnv
  where

    -- lookup/evaluate the functor and params
    fMod@(C.FunctorMod funArgs funExprs funModData) = modEnv Map.! fModId
    argMods = map ((Map.!) modEnv) argModIds

    -- rename the argMods according to pos/ create a new modenv to evalulate the applciation within
    -- TODO - test that the args are all LitMods / evalulated to LitMods
    appModEnv :: C.ModuleEnv
    appModEnv = Map.fromList $ zip (OrdMap.keys funArgs) argMods

