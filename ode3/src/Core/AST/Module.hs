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
Module(..), ModuleData(..), ModuleEnv, ExprMap, TypeMap, IdBimap,
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

-- | bidirectional map between internal ids and source ids
type IdBimap = Bimap.Bimap UntypedId SrcId
type ExprMap a = OrdMap.OrdMap (Bind a) (Top a)
type TypeMap = Map.Map SrcId Type

-- | Main executable modules that can be combined at run-time, they represent a dform of the simply-typed \-calc that is interpreted at runtime
-- type-checking occurs in two-stage process, vars and abs are checked during parsing, applications are cehcked from the replicate
-- var modeules must be closed anfd fully typered, abs/parameterisd modules are open (wrt to parameters) and may be polymorphic
data Module a = VarMod SrcId (ExprMap a) ModuleData -- (Name, Expr, Type, Bimap, LastId)
                | AbsMod SrcId [Module a] (ExprMap a) ModuleData -- (Name, ModArgs, Expr, Type, Bimap, LastId)
                -- we never have access to the appmodules, they are always immediatly applied and the resulting ClosedModule is saved under this name
                | AppMod SrcId SrcId SrcId
                deriving (Show, Eq)

-- | Metadata regarding a module
data ModuleData =   ModuleData {tMap :: TypeMap, idBimap :: IdBimap, maxId :: Maybe UntypedId}
                    deriving (Show, Eq)

-- | Module envirnomne,t the run-time envirmornet used to create models and start simulations, holds the current results from interpreting the module system
type ModuleEnv = Map.Map SrcId (Module (Id TypedId))


