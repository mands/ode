-----------------------------------------------------------------------------
--
-- Module      :  Core.Module.AST
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  BSD3
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Module system, represents either a closed, complete module or an open, parameterised model
--
-----------------------------------------------------------------------------

{-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures, FlexibleInstances, TypeSynonymInstances #-}

module Ode.AST.Module(
OdeTopElem(..),
ExprMap, ExprList, FunArgs,
Module(..), getModExprs, putModExprs, modifyModExprs,
GlobalModEnv, LocalModEnv, FileData(..), mkFileData, replModRoot,
getModuleMod, getModuleFile, getModuleGlobal,

getFileData, lookupModSig, getVarId, lookupModId, calcSigMap,
addTypesToExpr, getTypesFromExpr, updateModData1, updateModData2,

ModData(..), mkModData, getModData, putModData, modifyModData,
SigMap, TypeMap, IdMap

) where

import Control.Monad
import Control.Applicative
import Control.Monad.Error
import Data.Monoid
import qualified Data.Traversable as DT
import qualified Data.Foldable as DF
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Bimap as Bimap
import Data.Maybe (fromJust)
import Text.Printf (printf)


import AST.Common
import qualified AST.Core as E
import qualified Subsystem.Units as U
import qualified Utils.OrdMap as OrdMap
import Ode.Utils.CommonImports
import qualified System.FilePath as FP


-- Module/File Datatype ------------------------------------------------------------------------------------------------

-- | Top level module variables, represent the ast for an individual file, inc import cmds and module defs
data OdeTopElem a   = TopModDef ModRoot ModName (Module a)  -- top level module def, inluding root and name
                    | TopModImport ModImport                -- top level import
                    deriving (Show, Eq, Ord)

-- Module Datatype -----------------------------------------------------------------------------------------------------

-- | Main executable modules that can be combined at run-time, they represent a form of the simply-typed \-calc that is interpreted at runtime
-- type-checking occurs in two-stage process, vars and abs are checked during parsing, applications are cehcked from the replicate
-- var modeules must be closed anfd fully typered, abs/parameterisd modules are open (wrt to parameters) and may be polymorphic
data Module a = LitMod (ModData a)
                | FunctorMod FunArgs (ModData a)
                | AppMod ModName [Module a]     -- we never have access to the appmodules,
                                                -- they are always immediatly applied and the resulting ClosedModule is saved under this name
                | VarMod ModName            -- only used within appmods (and var refs)

                -- this holds a ref/thunk to a previously evaled module, (used for AppMod and VarMod)
                -- RefMod -> name, isClosed?, lifted mod sigMap, adjusted/cumulative modEnv
                | RefMod ModFullName Bool SigMap LocalModEnv
                deriving (Show, Eq, Ord)

{-- Not yet implemented/needed
data RefModType = RefLit
                | RefFunc
                | RefEvalApp SigMap LocalModEnv
                deriving (Show, Eq, Ord)
--}

-- Module Body Data
type ExprList = [E.TopLet DesId]

-- | ExprMap is the basic collection of expressions that make up a module
type ExprMap a = OrdMap.OrdMap [a] (E.TopLet a)

-- | FunArgs are the list of module parameters, and thier required signatures, for a functor application
-- is initially populated with just the modname/args by the parser, then filled with sigMaps during type-checking
type FunArgs = OrdMap.OrdMap ModName SigMap

-- | SigMap is the external signature/typemap for the module - is created on demand
type SigMap = Map.Map SrcId E.Type
-- | map between externally visible source ids and module internal ids
type IdMap = Map.Map SrcId Id
-- | Typemap is the internal typemap for all vars (top and expr) within a module
type TypeMap = Map.Map Id E.Type -- maybe switch to IntMap?

-- | Metadata regarding a lit/func module
data ModData a = ModData    { modExprMap :: ExprMap a, modIdMap :: IdMap, modTMap :: TypeMap
                            , modFreeId :: Id, modFullName :: ModFullName, modModEnv :: LocalModEnv
                            -- tmp data
                            , modImportCmds :: [ModImport], modExprList :: ExprList, modExportSet :: Set.Set SrcId
                            -- units data
                            , modQuantities :: U.Quantities, modUnits :: [U.UnitDef], modDerived :: U.DerivedUnits, modConvs :: [U.ConvDef]
                            } deriving (Show, Eq, Ord)

mkModData = ModData { modExprMap = OrdMap.empty, modIdMap = Map.empty, modTMap = Map.empty
                    , modFreeId = 0, modFullName = ModFullName (mkModRoot []) (ModName "<empty>"), modModEnv = Map.empty
                    -- tmp data
                    , modImportCmds = [], modExprList = [], modExportSet = Set.empty
                    -- units data
                    , modQuantities = [], modUnits = [], modDerived = [], modConvs = []
                    }

-- Module ModData accessors
getModData :: Module a -> Maybe (ModData a)
getModData (LitMod modData) = Just modData
getModData (FunctorMod _ modData) = Just modData
-- getModData (RefMod _ _ modData) = Just modData
getModData mod = Nothing

putModData :: Module a -> ModData a -> Module a
putModData (LitMod _) modData' = LitMod modData'
putModData (FunctorMod args _) modData' = FunctorMod args modData'
-- putModData (RefMod isClosed modFullName _) modData' = RefMod isClosed modFullName modData'
putModData mod _ = mod

modifyModData :: (ModData a -> ModData a) -> Module a -> Module a
modifyModData f m = maybe m (\md -> putModData m (f md)) $ getModData m

getModExprs :: Module a -> Maybe (ExprMap a)
getModExprs mod = modExprMap <$> getModData mod

putModExprs :: Module a -> ExprMap a -> Module a
putModExprs mod exprMap = modifyModData (\modData -> modData { modExprMap = exprMap }) mod

modifyModExprs :: (ExprMap a -> ExprMap a) -> Module a -> Module a
modifyModExprs f m = maybe m (\md -> putModExprs m (f md)) $ getModExprs m

-- Module-Level ModData Functions --------------------------------------------------------------------------------------

-- Lookup the type of a binding within a module type-signature
lookupModSig :: SrcId -> Module Id -> MExcept E.Type
lookupModSig v mod = do
    sigMap <- case mod of
        (RefMod _ True sigMap _)    -> return sigMap
        (LitMod modData)            -> return (calcSigMap modData)
        _                           -> throwError "(MD) Module does not have a type signature"
    maybeToExcept (Map.lookup v sigMap) $ printf "(MD) Binding %s not found in module" (show v)

calcSigMap :: ModData a -> SigMap
calcSigMap modData = Map.map (\id -> (modTMap modData) Map.! id) (modIdMap modData)

lookupModId :: E.SrcId -> Module Id -> MExcept Id
lookupModId v mod = do
    idMap <- maybeToExcept (modIdMap <$> getModData mod) "(MD) Module does not contain modData"
    maybeToExcept (Map.lookup v idMap) $ printf "(MD) Binding %s not found in module" (show v)

getVarId :: E.VarId E.Id -> ModFullName -> GlobalModEnv -> MExcept Id
getVarId lv@(E.LocalVar v) _ _ = return v
-- need lookup id of v within modname
getVarId (E.ModVar m v) modName gModEnv = do
    mod <- getModuleGlobal modName gModEnv
    lookupModId v mod


-- | Update the in-band type-annotations within the exprmap within the calculated typeMap
addTypesToExpr :: ExprMap E.Id -> TypeMap -> ExprMap E.Id
addTypesToExpr exprMap tMap = fmap addTypesTop exprMap
  where
    addTypesTop :: (E.TopLet E.Id) -> (E.TopLet E.Id)
    addTypesTop (E.TopLet isSVal _ (b:[]) e) = E.TopLet isSVal (tMap ! b) [b] (addTypesExpr e)
    addTypesTop (E.TopLet isSVal _ bs e) = E.TopLet isSVal (E.TTuple (map (\b -> tMap ! b) bs)) bs (addTypesExpr e)
    addTypesTop tl = tl

    addTypesExpr :: (E.Expr E.Id) -> (E.Expr E.Id)
    addTypesExpr (E.Let isSVal _ (b:[]) e1 e2) = E.Let isSVal (tMap ! b) [b] (addTypesExpr e1) (addTypesExpr e2)
    addTypesExpr (E.Let isSVal _ bs e1 e2) = E.Let isSVal (E.TTuple (map (\b -> tMap ! b) bs)) bs (addTypesExpr e1) (addTypesExpr e2)
    addTypesExpr e = E.mapExpr addTypesExpr e

getTypesFromExpr :: ExprMap E.Id -> TypeMap
getTypesFromExpr exprMap = DF.foldl addTypesTop  Map.empty exprMap
  where
    addTypesTop :: TypeMap -> (E.TopLet E.Id) -> TypeMap
    addTypesTop tMap (E.TopLet isSVal t (b:[]) e) = Map.insert b t tMap |> (\tMap -> addTypesExpr tMap e)
    addTypesTop tMap (E.TopLet isSVal (E.TTuple ts) bs e) = foldl (\tMap (b, t) -> Map.insert b t tMap) tMap (zip bs ts) |> (\tMap -> addTypesExpr tMap e)
    addTypesTop tMap tl = tMap

    addTypesExpr :: TypeMap -> (E.Expr E.Id) -> TypeMap
    addTypesExpr tMap (E.Let isSVal t (b:[]) e1 e2) = Map.insert b t tMap |> (\tMap -> addTypesExpr tMap e1) |> (\tMap -> addTypesExpr tMap e2)
    addTypesExpr tMap (E.Let isSVal (E.TTuple ts) bs e1 e2) = foldl (\tMap (b, t) -> Map.insert b t tMap) tMap (zip bs ts)
        |> (\tMap -> addTypesExpr tMap e1) |> (\tMap -> addTypesExpr tMap e2)
    addTypesExpr tMap e = E.foldExpr addTypesExpr tMap e

-- Need to figure out the correct abstactions here
-- | Updates the module data using new type infromation, hence updates exprmap info too
updateModData1 :: ModData E.Id -> TypeMap -> ModData E.Id
updateModData1 modData tMap = modData { modTMap = tMap, modExprMap = addTypesToExpr (modExprMap modData) tMap }

-- | Updates the module data using new exprMap infromation, hence updates tMap info too
updateModData2 :: ModData E.Id -> ExprMap E.Id -> ModData E.Id
updateModData2 modData exprMap = modData { modExprMap = exprMap, modTMap = getTypesFromExpr exprMap }


-- Module Environments -------------------------------------------------------------------------------------------------

-- | Global module env, a miror of the mod URI strcuture, first indexed by the modRoot,
-- then the modName, returning the individual module after
type GlobalModEnv = Map.Map ModRoot FileData
-- | Local/File module env, used to hold modules currently avialable within the module/file
type LocalModEnv = Map.Map ModName (Module Id)

-- | Metadata regarding a file (we treat the console/REPL env as a special in-memory file)
-- we store the fileRoot as well as is needed for modules to know their own location/path
data FileData = FileData { fileModEnv :: LocalModEnv, fileModRoot :: ModRoot } deriving (Show, Eq)
-- | Makes an empty fileData object, requires a ModRoot
mkFileData = FileData Map.empty
-- special datatype used to hold in-memory REPL file
replModRoot = mkModRoot ["<console>"]

-- Module Lookups ------------------------------------------------------------------------------------------------------
-- | Top level function that resolves a module lookup at the module and global level,
-- (NOTE - this does not look at the file-level, instead assume that all file-level refs,
-- including within a file, can be resolved using full-name within global env)
getModuleMod :: ModName -> ModData a -> GlobalModEnv -> MExcept (ModFullName, Module Id)
getModuleMod modName modData gModEnv = getModule' modName mModEnv Nothing gModEnv
  where
    mModEnv = modModEnv modData

-- | Top level function that resolves a module lookup at the file and global level,
-- looks first in the local file env, the globalenv using importlist
-- returns the full modname and the module itself, where the fullname may point to the same initial filedata
getModuleFile :: ModName -> FileData -> GlobalModEnv -> MExcept (ModFullName, Module Id)
getModuleFile modName fileData gModEnv = getModule' modName fModEnv mFModRoot gModEnv
  where
    fModEnv = fileModEnv fileData
    mFModRoot = Just $ fileModRoot fileData

-- | An internal function that takes a mod name, a local env and import map (both either mod/file level), and the global env and reolves the lookup
-- returns both the module and, if available, the modules fully qualified name
getModule' :: ModName -> LocalModEnv -> Maybe ModRoot -> GlobalModEnv -> MExcept (ModFullName, Module Id)
getModule' modName modEnv mModRoot gModEnv = case (Map.lookup modName modEnv) of -- look within modEnv
    -- we only return a modfullname if it makes sense - i.e. is linked to a file root
    Just mod    -> return $ maybe (ModLocalName modName, mod) (\modRoot -> (ModFullName modRoot modName, mod)) mModRoot
    Nothing     -> throwError $ printf "Unknown reference to module %s within file (maybe missing an import)" (show modName)


-- | Retreive a module from the global modEnv
-- ModEnv helper functions
getModuleGlobal :: ModFullName -> GlobalModEnv -> MExcept (Module Id)
getModuleGlobal (ModFullName modRoot modName) gModEnv =
    maybeToExcept mMod $ printf "Referenced module %s.%s not found or currently loaded in global envirnoment" (show modRoot) (show modName)
  where
    mMod = Map.lookup modRoot gModEnv >>= (\fileData -> Map.lookup modName (fileModEnv fileData))
getModuleGlobal (ModLocalName modName) gModEnv = throwError $ printf "Cannot retrieve global module for a local name %s" (show modName)


-- | Returns the filedata for a particular modroot
getFileData :: ModRoot -> GlobalModEnv -> MExcept FileData
getFileData modRoot modEnv = maybeToExcept (Map.lookup modRoot modEnv) $ printf "Module root %s not found or currently loaded" (show modRoot)

-- | Returns the filedata for a particvular modroot, if it doesn't exist then one is created
getCreateFileData :: ModRoot -> GlobalModEnv -> FileData
getCreateFileData modRoot modEnv = Map.findWithDefault (mkFileData $ mkModRoot ["<empty>"]) modRoot modEnv


-- Misc Functions ------------------------------------------------------------------------------------------------------

-- need to put more helper functions here
-- for instance functions to union two exprMaps, modules, remap ids, etc.
