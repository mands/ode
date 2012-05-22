-----------------------------------------------------------------------------
--
-- Module      :  Core.AST.Module
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

module Lang.Module.AST (
OdeTopElem(..), ModImport(..),
ExprMap, ExprList, FunArgs,
Module(..),
GlobalModEnv, LocalModEnv, FileData(..), mkFileData, getGlobalMod, getFileData, ImportMap,
ModData(..), mkModData, SigMap, TypeMap, IdBimap, debugModuleExpr,
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

import Lang.Common.AST
import qualified Lang.Core.AST as E
import qualified Utils.OrdMap as OrdMap
import Utils.Utils
import qualified System.FilePath as FP


-- | Top level module variables, represent the ast for an individual file, inc import cmds and module defs
data OdeTopElem a   = TopModDef ModRoot ModName (Module a)  -- top level module def, inluding root and name
                    | TopModImport ModImport                -- top level import
                    -- | ModAlias ModName ModName              -- an alias from one ModURI to another
                    deriving (Show, Eq, Ord)


-- | import cmds, has a module root/filename, and list of indiv modules and potential alias
data ModImport = ModImport ModRoot (Maybe [(ModName, Maybe ModName)]) deriving (Show, Eq, Ord)

-- Module Body Data

type ExprList = [E.TopLet DesId]


-- | ExprMap is the basic collection of expressions that make up a module
type ExprMap a = OrdMap.OrdMap (E.Bind a) (E.TopLet a)

-- | FunArgs are the list of module parameters, and thier required signatures, for a functor application
type FunArgs = OrdMap.OrdMap ModName SigMap

-- | Main executable modules that can be combined at run-time, they represent a form of the simply-typed \-calc that is interpreted at runtime
-- type-checking occurs in two-stage process, vars and abs are checked during parsing, applications are cehcked from the replicate
-- var modeules must be closed anfd fully typered, abs/parameterisd modules are open (wrt to parameters) and may be polymorphic
data Module a = LitMod  (ExprMap a) ModData
                | FunctorMod FunArgs (ExprMap a) ModData
                | AppMod ModName [Module a]     -- we never have access to the appmodules,
                                                -- they are always immediatly applied and the resulting ClosedModule is saved under this name
                | VarMod ModFullName            -- only used within appmods
                deriving (Show, Eq, Ord)

-- | Metadata regarding a module
data ModData = ModData  { modSig :: SigMap, modTMap :: TypeMap, modIdBimap :: IdBimap, modFreeId :: Maybe Id
                        , modImportMap :: ImportMap, modImportCmds :: [ModImport]
                        , modExprList :: ExprList
                        } deriving (Show, Eq, Ord)

mkModData = ModData Map.empty Map.empty Bimap.empty Nothing Map.empty [] []

-- | bidirectional map between internal ids and source ids for all visible/top-level defined vars
type IdBimap = Bimap.Bimap SrcId Id
-- | SigMap is the external typemap for the module - can be created from the typemap, top-level expressions and idbimap
type SigMap = Map.Map SrcId E.Type
-- | Typemap is the internal typemap for all vars (top and expr) within a module
type TypeMap = Map.Map Id E.Type -- maybe switch to IntMap?


-- | Global module env, a miror of the mod URI strcuture, first indexed by the modRoot,
-- then the modName, returning the individual module after
type GlobalModEnv = Map.Map ModRoot FileData

-- | Local module env, used to hold modules currently accessible in scope
-- Module environment the run-time envirmornet used to create models and start simulations, holds the current results from interpreting the module system
type LocalModEnv = Map.Map ModName (Module Id)

type ImportMap = Map.Map ModName ModFullName


-- | Metadata regarding a file (we treat the console env as a special file)
data FileData = FileData { fileImportMap :: ImportMap, fileModEnv :: LocalModEnv } deriving (Show, Eq)

mkFileData = FileData Map.empty Map.empty

-- ModEnv helper functions
-- | Retreive a module from the global modEnv
getGlobalMod :: GlobalModEnv -> ModFullName -> MExcept (Module Id)
getGlobalMod modEnv modFullName =
    maybeToExcept mMod $ "Module " ++ show modFullName ++ " not found or currently loaded"
  where
    -- fullName = mkModFullName (Just modRoot) modName
    (mModRoot, modName) = splitModFullName modFullName
    mMod = do
        modRoot <- mModRoot
        fileData <- Map.lookup modRoot modEnv
        Map.lookup modName (fileModEnv fileData)

-- | Returns the filedata for a particvular modroot
getFileData :: GlobalModEnv -> ModRoot -> MExcept FileData
getFileData modEnv modRoot = maybeToExcept (Map.lookup modRoot modEnv) $ "Module root " ++ show modRoot ++ " not found or currently loaded"

-- | Returns the filedata for a particvular modroot, if it doesn't exist then one is created
getCreateFileData :: GlobalModEnv -> ModRoot -> FileData
getCreateFileData modEnv modRoot = Map.findWithDefault mkFileData modRoot modEnv

--getFileModEnv :: GlobalModEnv -> ModRoot -> FileData
--getFileModEnv



-- need to put more helper functions here
-- for instance functions to union two exprMaps, modules, remap ids, etc.
debugModuleExpr :: (Show a) => Module a -> String
debugModuleExpr (LitMod exprMap _) = prettyPrint exprMap
debugModuleExpr (FunctorMod _ exprMap _) = prettyPrint exprMap
debugModuleExpr (AppMod _ _) = "Application - no exprs"


instance (Show a) => PrettyPrint (OdeTopElem a) where
    prettyPrint (TopModDef root name mod) = show root ++ "." ++ show name ++ " :: " ++ prettyPrint mod
    prettyPrint _ = undefined

instance (Show a) => PrettyPrint (Module a) where
    -- show the module signature
    prettyPrint (LitMod exprMap modData) = "Closed :: " ++ prettyPrint modData

    -- show the args and module sig
    prettyPrint (FunctorMod funcArgs exprMap modData) = "Functor :: (" ++ prettyPrint funcArgs ++ ") -> " ++ prettyPrint modData

    -- show the functor and args
    prettyPrint mod@(AppMod functor args) = "Application - " ++ show functor ++ "(" ++ show args ++ ")"

-- show the module signature
instance PrettyPrint (ModData) where
    prettyPrint (ModData sig _ _ _ _ _ _) = show sig

instance PrettyPrint FunArgs where
    prettyPrint funArgs = List.intercalate "," $ List.map (\(m, sig) -> show m ++ " :: {" ++ show sig ++ "}") (OrdMap.toList funArgs)


