-----------------------------------------------------------------------------
--
-- Module      :  Core.Module.AST
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Module system, represents either a closed, complete module or an open, parameterised model
--
-----------------------------------------------------------------------------

{-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures, FlexibleInstances, TypeSynonymInstances #-}

module AST.Module(
OdeTopElem(..),
ExprMap, ExprList, FunArgs,
Module(..), getModExprs, putModExprs, modifyModExprs,
GlobalModEnv, LocalModEnv, FileData(..), mkFileData, replModRoot,
getModuleMod, getModuleFile, getModuleGlobal,
getFileData, lookupModSig, getVarSrcName,
ModData(..), mkModData, getModData, putModData, modifyModData,
SigMap, TypeMap, IdBimap
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
import Utils.CommonImports
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
data Module a = LitMod  (ExprMap a) ModData
                | FunctorMod FunArgs (ExprMap a) ModData
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

-- | bidirectional map between internal ids and source ids for all visible/top-level defined vars
type IdBimap = Bimap.Bimap SrcId Id
-- | SigMap is the external typemap for the module - can be created from the typemap, top-level expressions and idbimap
type SigMap = Map.Map SrcId E.Type
-- | Typemap is the internal typemap for all vars (top and expr) within a module
type TypeMap = Map.Map Id E.Type -- maybe switch to IntMap?

-- TODO - add explicity export lists
-- | Metadata regarding a module
data ModData = ModData  { modSigMap :: SigMap, modTMap :: TypeMap, modIdBimap :: IdBimap, modFreeId :: Maybe Id
                        , modFullName :: ModFullName, modModEnv :: LocalModEnv
                        , modImportCmds :: [ModImport], modExportSet :: Set.Set SrcId, modExprList :: ExprList
                        , modQuantities :: U.Quantities, modUnits :: [U.UnitDef], modConvs :: [U.ConvDef]
                        } deriving (Show, Eq, Ord)

mkModData = ModData     { modSigMap = Map.empty, modTMap = Map.empty, modIdBimap = Bimap.empty, modFreeId = Nothing
                        , modFullName = ModFullName (mkModRoot []) (ModName "<empty>"), modModEnv = Map.empty
                        , modImportCmds = [], modExportSet = Set.empty, modExprList = []
                        , modQuantities = [], modUnits = [], modConvs = []
                        }

-- Module ModData accessors
getModData :: Module a -> Maybe ModData
getModData (LitMod _ modData) = Just modData
getModData (FunctorMod _ _ modData) = Just modData
-- getModData (RefMod _ _ modData) = Just modData
getModData mod = Nothing

putModData :: Module a -> ModData -> Module a
putModData (LitMod exprMap _) modData' = LitMod exprMap modData'
putModData (FunctorMod args exprMap _) modData' = FunctorMod args exprMap modData'
-- putModData (RefMod isClosed modFullName _) modData' = RefMod isClosed modFullName modData'
putModData mod _ = mod

modifyModData :: Module a -> (ModData -> ModData) -> Module a
modifyModData m f = maybe m (\md -> putModData m (f md)) $ getModData m

getModExprs :: Module a -> Maybe (ExprMap a)
getModExprs (LitMod exprMap _) = Just exprMap
getModExprs (FunctorMod _ exprMap _) = Just exprMap
getModExprs mod = Nothing

putModExprs :: Module a -> ExprMap a -> Module a
putModExprs (LitMod _ modData) exprMap' = LitMod exprMap' modData
putModExprs (FunctorMod args _ modData) exprMap' = FunctorMod args exprMap' modData
putModExprs mod _ = mod

modifyModExprs :: Module a -> (ExprMap a -> ExprMap a) -> Module a
modifyModExprs m f = maybe m (\md -> putModExprs m (f md)) $ getModExprs m

-- Module-Level ModData Functions --------------------------------------------------------------------------------------

-- Lookup the type of a binding within a module type-signature
lookupModSig :: SrcId -> Module Id -> MExcept E.Type
lookupModSig v mod = maybeToExcept lookupM $ printf "(MD) Binding %s not found in module" (show v)
  where
    lookupM = do
        sigMap <- case mod of
            (RefMod _ True sigMap _)    -> return sigMap
            (LitMod _ modData)          -> return (modSigMap modData)
            _                           -> Nothing
        Map.lookup v sigMap


getVarSrcName :: E.VarId E.Id -> ModData -> SrcId
getVarSrcName lv@(E.LocalVar v) modData = (modIdBimap modData) Bimap.!> v
getVarSrcName mv@(E.ModVar _ v) _ = v


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
getModuleMod :: ModName -> ModData -> GlobalModEnv -> MExcept (ModFullName, Module Id)
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
