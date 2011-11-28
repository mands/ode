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

module Core.ModuleAST (
ModImport, TopMod(..), Module(..), ModuleData(..), ModuleEnv, ExprMap, SigMap, TypeMap, FunArgs, IdBimap, debugModuleExpr,
SafeExprMap, insertTopExpr, emptySafeExprMap, convertExprMap
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

import Common.AST
import qualified Core.ExprAST as E
import qualified Utils.OrdMap as OrdMap
import Utils.Utils

-- TODO - questions
-- how does interpretation proceed, is strict, hence order determined by pos in file
-- need to add polymorphism to type-checker/impletemet two-stage type-checking
-- how does the renamer work - do we need two typed of ids, locaql and module level - do we rename the modules too?
-- ??

-- | import created by import directive
type ModImport = [String]
-- data ModImport = ModImport String (Maybe String) deriving (Show, Eq, Ord)

-- | Top level module variables
data TopMod a = TopMod E.SrcId (Module a) deriving (Show, Eq)

type ExprMap a = OrdMap.OrdMap (E.Bind a) (E.Top a)

-- | FunArgs are the list of module parameters, and thier required signatures, for a functor application
type FunArgs = OrdMap.OrdMap SrcId SigMap

-- | Main executable modules that can be combined at run-time, they represent a form of the simply-typed \-calc that is interpreted at runtime
-- type-checking occurs in two-stage process, vars and abs are checked during parsing, applications are cehcked from the replicate
-- var modeules must be closed anfd fully typered, abs/parameterisd modules are open (wrt to parameters) and may be polymorphic
data Module a = LitMod  (ExprMap a) ModuleData -- Expr, ModType, IntType, Bimap, LastId)
                | FunctorMod FunArgs (ExprMap a) ModuleData -- ModArgs, Expr, Type, Bimap, LastId)
                -- we never have access to the appmodules, they are always immediatly applied and the resulting ClosedModule is saved under this name
                | AppMod E.SrcId [Module a]
                -- only used within appmods
                | VarMod SrcId
                deriving (Show, Eq)

-- | Metadata regarding a module
data ModuleData =   ModuleData {modSig :: SigMap, modTMap :: TypeMap, modIdBimap :: IdBimap, modFreeId :: Maybe Id}
                    deriving (Show, Eq)

-- | bidirectional map between internal ids and source ids for all visible/top-level defined vars
type IdBimap = Bimap.Bimap SrcId Id
-- | SigMap is the external typemap for the module - can be created from the typemap, top-level expressions and idbimap
type SigMap = Map.Map SrcId E.Type
-- | Typemap is the internal typemap for all vars (top and expr) within a module
type TypeMap = Map.Map Id E.Type -- maybe switch to IntMap?


-- | Module environment the run-time envirmornet used to create models and start simulations, holds the current results from interpreting the module system
type ModuleEnv = Map.Map SrcId (Module Id)

-- need to put more helper functions here
-- for instance functions to union two exprMaps, modules, remap ids, etc.
debugModuleExpr :: (Show a) => Module a -> String
debugModuleExpr (LitMod exprMap _) = prettyPrint exprMap
debugModuleExpr (FunctorMod _ exprMap _) = prettyPrint exprMap
debugModuleExpr (AppMod _ _) = "Application - no exprs"


-- TODO - move this error detection elsewhere, perhaps into validator
newtype SafeExprMap a = SafeExprMap (SafeExprMapC a)
data SafeExprMapC a = SafeExprMapC (Set.Set a) (ExprMap a)

emptySafeExprMap :: SafeExprMap a
emptySafeExprMap = SafeExprMap (SafeExprMapC Set.empty OrdMap.empty)

-- | Util func to safely insert a top expression into an expremap with the given binding,
-- throws an exception if the binding exists in the map
insertTopExpr :: (Show a, Ord a) => E.Top a -> SafeExprMap a -> MExcept (SafeExprMap a)
insertTopExpr topExpr (SafeExprMap (SafeExprMapC curBinds exprMap)) = case topExpr of
    (E.TopAbs b'@(E.AbsBind b) _ _) -> liftA SafeExprMap $ SafeExprMapC <$> (addBinding curBinds b) <*> addExprMap' b'
    (E.TopLet b'@(E.LetBind bs) _) -> liftA SafeExprMap $ SafeExprMapC <$> (DF.foldlM addBinding curBinds bs) <*> addExprMap' b'
  where
    addBinding curBinds b = case Set.member b curBinds of
        True -> throwError $ "(MO01) - Top-level binding " ++ (show b) ++ " already exists in module"
        False -> pure $ (Set.insert b curBinds)
    addExprMap' b = pure (OrdMap.insert b topExpr exprMap)

convertExprMap :: SafeExprMap a -> ExprMap a
convertExprMap (SafeExprMap (SafeExprMapC curBinds exprMap)) = exprMap


-- standard typeclass instances
-- | Make LitMod a instnace of monoid, where mappend a b imppiles adding the module b to occur after module a
instance Monoid (Module Id) where
    mempty = LitMod OrdMap.empty mempty

    mappend modA@(LitMod exprMapA modDataA) modB@(LitMod exprMapB modDataB) = maybe modA appendMods freeId'
      where
        freeIdA = modFreeId modDataA
        freeIdB = modFreeId modDataB
        freeId' = liftM2 (+) freeIdA freeIdB

        -- append the mods using the deltaId
        appendMods deltaId = LitMod exprMap' modDataA
          where
            exprMap' = OrdMap.map updateIds exprMapB
            updateIds (topB, topExpr) = (fmap (+ deltaId) topB, fmap (+ deltaId) topExpr)
            modData' = modDataA `mappend` modDataB


instance Monoid ModuleData where
    mempty = (ModuleData { modSig = Map.empty, modTMap = Map.empty, modIdBimap = Bimap.empty, modFreeId = Nothing })
    mappend a b = a
      where
        modSig' = undefined

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
    prettyPrint (ModuleData sig tMap idBimap mFreeId) = show sig

instance PrettyPrint FunArgs where
    prettyPrint funArgs = List.intercalate "," $ List.map (\(m, sig) -> show m ++ " :: {" ++ show sig ++ "}") (OrdMap.toList funArgs)
















