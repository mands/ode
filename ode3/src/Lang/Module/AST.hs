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
ModCmd(..), ModURIElems,
ExprMap, ExprList, FunArgs,
ModURI, TopMod(..), Module(..), ModuleEnv,
ModuleData(..), SigMap, TypeMap, IdBimap, debugModuleExpr,
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

-- 2nd level mod cmds AST, should combine with orig Module AST cmds
data ModCmd = ModImport ModURIElems (Maybe String)  -- sep'd mod elems, alias
            | ModAlias ModURI ModURI                -- an alias from one ModURI to another
            deriving Show

type ModURIElems = [ModURI]



-- | a canoical module name
type ModURI = String
-- data ModImport = ModImport String (Maybe String) deriving (Show, Eq, Ord)

-- Module Body Data

type ExprList = [E.TopLet E.DesId]
-- | ExprMap is the basic collection of expressions that make up a module
type ExprMap a = OrdMap.OrdMap (E.Bind a) (E.TopLet a)

-- | FunArgs are the list of module parameters, and thier required signatures, for a functor application
type FunArgs = OrdMap.OrdMap SrcId SigMap

-- | Top level module variables
data TopMod a = TopMod E.SrcId (Module a) deriving (Show, Eq)

-- | Main executable modules that can be combined at run-time, they represent a form of the simply-typed \-calc that is interpreted at runtime
-- type-checking occurs in two-stage process, vars and abs are checked during parsing, applications are cehcked from the replicate
-- var modeules must be closed anfd fully typered, abs/parameterisd modules are open (wrt to parameters) and may be polymorphic
data Module a = LitMod  (ExprMap a) ModuleData
                | FunctorMod FunArgs (ExprMap a) ModuleData
                | AppMod E.SrcId [Module a]     -- we never have access to the appmodules,
                                                -- they are always immediatly applied and the resulting ClosedModule is saved under this name
                | VarMod ModURI                  -- only used within appmods
                deriving (Show, Eq)

-- | Metadata regarding a module
data ModuleData =   ModuleData {modSig :: SigMap, modTMap :: TypeMap, modIdBimap :: IdBimap, modFreeId :: Maybe Id, modExprList :: ExprList}
                    deriving (Show, Eq)


-- | bidirectional map between internal ids and source ids for all visible/top-level defined vars
type IdBimap = Bimap.Bimap SrcId Id
-- | SigMap is the external typemap for the module - can be created from the typemap, top-level expressions and idbimap
type SigMap = Map.Map SrcId E.Type
-- | Typemap is the internal typemap for all vars (top and expr) within a module
type TypeMap = Map.Map Id E.Type -- maybe switch to IntMap?


-- | Module environment the run-time envirmornet used to create models and start simulations, holds the current results from interpreting the module system
type ModuleEnv = Map.Map ModURI (Module Id)

-- need to put more helper functions here
-- for instance functions to union two exprMaps, modules, remap ids, etc.
debugModuleExpr :: (Show a) => Module a -> String
debugModuleExpr (LitMod exprMap _) = prettyPrint exprMap
debugModuleExpr (FunctorMod _ exprMap _) = prettyPrint exprMap
debugModuleExpr (AppMod _ _) = "Application - no exprs"


instance (Show a) => PrettyPrint (TopMod a) where
    prettyPrint (TopMod name mod) = show name ++ " :: " ++ prettyPrint mod

instance (Show a) => PrettyPrint (Module a) where
    -- show the module signature
    prettyPrint (LitMod exprMap modData) = "Closed :: " ++ prettyPrint modData

    -- show the args and module sig
    prettyPrint (FunctorMod funcArgs exprMap modData) = "Functor :: (" ++ prettyPrint funcArgs ++ ") -> " ++ prettyPrint modData

    -- show the functor and args
    prettyPrint mod@(AppMod functor args) = "Application - " ++ functor ++ "(" ++ show args ++ ")"

-- show the module signature
instance PrettyPrint ModuleData where
    prettyPrint (ModuleData sig tMap idBimap mFreeId _) = show sig

instance PrettyPrint FunArgs where
    prettyPrint funArgs = List.intercalate "," $ List.map (\(m, sig) -> show m ++ " :: {" ++ show sig ++ "}") (OrdMap.toList funArgs)


