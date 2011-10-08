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

{-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures #-}

module Core.AST.Module (
TopMod(..), Module(..), ModuleData(..), ModuleEnv, ExprMap, SigMap, TypeMap, FunArgs, IdBimap,
) where

import qualified Data.Map as Map
import qualified Data.Bimap as Bimap
import Core.AST.Expr
import qualified Utils.OrdMap as OrdMap

-- TODO - questions
-- how does interpretation proceed, is strict, hence order determined by pos in file
-- need to add polymorphism to type-checker/impletemet two-stage type-checking
-- how does the renamer work - do we need two typed of ids, locaql and module level - do we rename the modules too?
-- ??

-- | Top level module variables
data TopMod a = TopMod SrcId (Module a) deriving (Show, Eq)

type ExprMap a = OrdMap.OrdMap (Bind a) (Top a)

-- | FunArgs are the list of module parameters, and thier required signatures, for a functor application
type FunArgs = OrdMap.OrdMap SrcId SigMap

-- | Main executable modules that can be combined at run-time, they represent a dform of the simply-typed \-calc that is interpreted at runtime
-- type-checking occurs in two-stage process, vars and abs are checked during parsing, applications are cehcked from the replicate
-- var modeules must be closed anfd fully typered, abs/parameterisd modules are open (wrt to parameters) and may be polymorphic
data Module a = LitMod  (ExprMap a) ModuleData -- Expr, ModType, IntType, Bimap, LastId)
                | FunctorMod FunArgs (ExprMap a) ModuleData -- ModArgs, Expr, Type, Bimap, LastId)
                -- we never have access to the appmodules, they are always immediatly applied and the resulting ClosedModule is saved under this name
                | AppMod SrcId [SrcId]
                -- | VarMod SrcId ??
                deriving (Show, Eq)

-- | Metadata regarding a module
data ModuleData =   ModuleData {modSig :: SigMap, modTMap :: TypeMap, modIdBimap :: IdBimap, modFreeId :: Maybe Id}
                    deriving (Show, Eq)

-- | bidirectional map between internal ids and source ids for all visible/top-level defined vars
type IdBimap = Bimap.Bimap SrcId Id
-- | SigMap is the external typemap for the module - can be created from the typemap, top-level expressions and idbimap
type SigMap = Map.Map SrcId Type
-- | Typemap is the internal typemap for all vars (top and expr) within a module
type TypeMap = Map.Map Id Type -- maybe switch to IntMap?


-- | Module environment the run-time envirmornet used to create models and start simulations, holds the current results from interpreting the module system
type ModuleEnv = Map.Map SrcId (Module Id)

--getModuleName :: Module a -> SrcId
--getModuleName (VarMod name _ _) = name
--getModuleName (AbsMod name _ _ _) = name
--getModuleName (AppMod name _ _) = name

-- need to put more helper functions here
-- for instance functions to union two exprMaps, modules, remap ids, etc.
