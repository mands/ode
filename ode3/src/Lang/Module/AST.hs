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

module Lang.Module.AST (
OdeTopElem(..), ModImport(..),
ExprMap, ExprList, FunArgs,
Module(..),
GlobalModEnv, FileModEnv, FileData(..), mkFileData, getGlobalMod, getFileData, ImportMap, getIdType,
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
import Text.Printf (printf)


import Lang.Common.AST
import qualified Lang.Core.AST as E
import qualified Utils.OrdMap as OrdMap
import Utils.Utils
import qualified System.FilePath as FP


-- Module/File Datatype ------------------------------------------------------------------------------------------------

-- | Top level module variables, represent the ast for an individual file, inc import cmds and module defs
data OdeTopElem a   = TopModDef ModRoot ModName (Module a)  -- top level module def, inluding root and name
                    | TopModImport ModImport                -- top level import
                    deriving (Show, Eq, Ord)

-- | import cmds, has a module root/filename, and list of indiv modules and potential alias
data ModImport = ModImport ModRoot (Maybe [(ModName, Maybe ModName)]) deriving (Show, Eq, Ord)


-- Module Datatype -----------------------------------------------------------------------------------------------------

-- | Main executable modules that can be combined at run-time, they represent a form of the simply-typed \-calc that is interpreted at runtime
-- type-checking occurs in two-stage process, vars and abs are checked during parsing, applications are cehcked from the replicate
-- var modeules must be closed anfd fully typered, abs/parameterisd modules are open (wrt to parameters) and may be polymorphic
data Module a = LitMod  (ExprMap a) ModData
                | FunctorMod FunArgs (ExprMap a) ModData
                | AppMod ModName [Module a]     -- we never have access to the appmodules,
                                                -- they are always immediatly applied and the resulting ClosedModule is saved under this name
                | VarMod ModName            -- only used within appmods
                deriving (Show, Eq, Ord)

-- Module Body Data
type ExprList = [E.TopLet DesId]

-- | ExprMap is the basic collection of expressions that make up a module
type ExprMap a = OrdMap.OrdMap (E.Bind a) (E.TopLet a)

-- | FunArgs are the list of module parameters, and thier required signatures, for a functor application
-- is initially populated with just the modname/args by the parser, then filled with sigMaps during type-checking
type FunArgs = OrdMap.OrdMap ModName SigMap

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


getIdType :: Module Id -> SrcId -> MExcept E.Type
getIdType mod v = maybeToExcept (Map.lookup v sigMap) $ printf "Binding %s not found in module" (show v)
  where
    sigMap = case mod of
        LitMod _ modData      -> modSig modData
        FunctorMod _ _ modData  -> modSig modData
        otherwise             -> Map.empty -- seems a bit hacky?


-- Module Environments -------------------------------------------------------------------------------------------------

-- | Global module env, a miror of the mod URI strcuture, first indexed by the modRoot,
-- then the modName, returning the individual module after
type GlobalModEnv = Map.Map ModRoot FileData

-- | File module env, used to hold modules currently avialable at the file level
type FileModEnv = Map.Map ModName (Module Id)

-- | Import map holds the modules imported by a file/module, and in scope
type ImportMap = Map.Map ModName ModFullName

-- | Metadata regarding a file (we treat the console/REPL env as a special in-memory file)
data FileData = FileData { fileImportMap :: ImportMap, fileModEnv :: FileModEnv } deriving (Show, Eq)

mkFileData = FileData Map.empty Map.empty

-- | Retreive a module from the global modEnv
-- ModEnv helper functions
getGlobalMod :: ModFullName -> GlobalModEnv -> MExcept (Module Id)
getGlobalMod modFullName gModEnv =
    maybeToExcept mMod $ "Referenced module " ++ show modFullName ++ " not found or currently loaded in global envirnoment"
  where
    -- fullName = mkModFullName (Just modRoot) modName
    (mModRoot, modName) = splitModFullName modFullName
    mMod = do
        modRoot <- mModRoot
        fileData <- Map.lookup modRoot gModEnv
        Map.lookup modName (fileModEnv fileData)

-- | Returns the filedata for a particvular modroot
getFileData :: GlobalModEnv -> ModRoot -> MExcept FileData
getFileData modEnv modRoot = maybeToExcept (Map.lookup modRoot modEnv) $ "Module root " ++ show modRoot ++ " not found or currently loaded"

-- | Returns the filedata for a particvular modroot, if it doesn't exist then one is created
getCreateFileData :: GlobalModEnv -> ModRoot -> FileData
getCreateFileData modEnv modRoot = Map.findWithDefault mkFileData modRoot modEnv


-- Misc Functions ------------------------------------------------------------------------------------------------------

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


