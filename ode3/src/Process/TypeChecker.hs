-----------------------------------------------------------------------------
--
-- Module      :  Core.Type2
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Type and unit-checker that uses HM algorithm
--
-- Auxillary Checks Performed
-- * Referenced (module) binding does exist (within modEnv scope)
-- * if import - is module in modEnv, and if so does var exist
-- * if functor - is mod name equal to a functor arg name

-----------------------------------------------------------------------------
{-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures #-}

module Process.TypeChecker (
typeCheck, -- , typeCheckApp, TypeVarEnv, TypeCons, unify
) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Error

import qualified Data.Map as Map
import qualified Data.Bimap as Bimap
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Foldable as DF
import qualified Data.Traversable as DT

import Utils.CommonImports
import Utils.MonadSupply
import qualified Utils.OrdMap as OrdMap

import qualified Subsystem.SysState as St
import AST.Common as AC
import qualified AST.Core as E
import qualified AST.Module as M
import qualified Subsystem.Units as U

import Process.TypeChecker.Common
import Process.TypeChecker.ConstraintGen
import Process.TypeChecker.Unification

-- Main Interface ------------------------------------------------------------------------------------------------------
typeCheck :: M.GlobalModEnv -> M.FileData -> St.UnitsState -> Bool -> U.Unit -> M.Module E.Id -> MExcept (M.Module E.Id)
typeCheck gModEnv fileData uState unitsCheck timeUnit mod@(M.LitMod modData) = do
    -- get the contraints
    ((TypeEnvs tEnv recRefEnv _), tCons) <- constrain gModEnv modData Nothing unitsCheck timeUnit
    -- unify the types and get the new typemap
    (tVEnv, uVEnv) <- unify uState tCons
    -- substitute to obtain the new type env
    tEnv' <- subTVars tEnv tVEnv uVEnv False
    -- split the typeEnvs
    let (tMap, _) = splitTypeEnvs tEnv'
    -- _ <- trace' [MkSB tEnv'] "Final TypeEnv" $ Right ()
    -- Update the module data with the new typemap
    return $ M.LitMod (M.updateModData1 modData tMap)

typeCheck gModEnv fileData uState unitsCheck timeUnit mod@(M.FunctorMod args modData) = do
    -- get the contraints
    ((TypeEnvs tEnv recRefEnv _), tCons) <- constrain gModEnv modData (Just args) unitsCheck timeUnit
    -- unify the types and get the new typemap`
    (tVEnv, uVEnv) <- unify uState tCons
    -- substitute to obtain the new type env
    tEnv' <- subTVars tEnv tVEnv uVEnv True
    let (tMap, mTEnv) = splitTypeEnvs tEnv'
    -- functor specific type-checking
    -- mTEnv' <- subTVars mTEnv tVEnv uVEnv True
    let args' = createFunModArgs args mTEnv
    -- Update the module data with the new typemap
    return $ M.FunctorMod args' (M.updateModData1 modData tMap)
  where
    -- create the public module signatures for Functors
    createFunModArgs :: M.FunArgs -> TypeEnv -> M.FunArgs
    createFunModArgs args mTEnv = Map.foldrWithKey addArg args mTEnv
      where
        -- add the type for M.v into the args OrdMap
        addArg (E.ModVar m v) t args = OrdMap.update updateModArgs m args
          where
            updateModArgs modMap = Just (Map.insert v t modMap)

-- | Take a typeenv and split both local and module-level type information
splitTypeEnvs :: TypeEnv -> (M.TypeMap, TypeEnv)
splitTypeEnvs tEnv = Map.foldrWithKey splitType (Map.empty, Map.empty) tEnv
  where
    splitType (E.LocalVar lv) t (tMap, mTEnv) = (Map.insert lv t tMap, mTEnv)
    splitType mv@(E.ModVar _ _) t (tMap, mTEnv) = (tMap, Map.insert mv t mTEnv)


-- | use the TVar map to undate a type enviroment (including both local and mod-refs) and substitute all TVars
-- Bool argument determinst wheter the checking should allow polymophism and not fully-unify
subTVars :: TypeEnv -> TypeVarEnv -> UnitVarEnv -> Bool -> MExcept TypeEnv
subTVars tEnv tVEnv uVEnv allowPoly = DT.mapM updateType tEnv
  where
    -- try to substitute a tvar if it exists - this will behave differently depending on closed/open modules
    updateType :: E.Type -> MExcept E.Type
    updateType t@(E.TVar i) = processType t $ Map.lookup i tVEnv
    updateType t@(E.TFloat (U.UnitVar uV)) = processType t $ E.TFloat <$> Map.lookup uV uVEnv
    updateType t = E.mapTypeM updateType t

    processType :: E.Type -> Maybe E.Type -> MExcept E.Type
    processType t (Just t')   = return t'
    processType t Nothing     = if allowPoly    then return t
                                                else trace' [MkSB tEnv, MkSB tVEnv, MkSB uVEnv] "Poly error" $
                                                        throwError "(TC03) - Type/Unit-variable found in non-polymorphic closed module"


