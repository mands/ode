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
-- | New type-checker that uses full HM algorithm - hopefully to be extedned to support higher-order
-- differntials and units
--
-- Auxillary Checks Performed
-- * Referenced (module) binding does exist (within import scope)
-- * if import - is module in importMap, and if so does var exist
-- * if functor - is mod name equal to a functor arg name

-----------------------------------------------------------------------------
{-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures #-}

module Lang.Core.TypeChecker (
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

import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Text.Printf (printf)

import Utils.Utils
import Utils.MonadSupply
import qualified Utils.OrdMap as OrdMap

import qualified SysState as St
import Lang.Common.AST
import qualified Lang.Core.AST as E
import qualified Lang.Module.AST as M
import qualified Lang.Core.Units as U

import Lang.Core.TypeChecker.Common
import Lang.Core.TypeChecker.ConstraintGen
import Lang.Core.TypeChecker.Unification

-- Main Interface ------------------------------------------------------------------------------------------------------
typeCheck :: M.GlobalModEnv -> M.FileData -> St.UnitsState -> M.Module E.Id -> MExcept (M.Module E.Id)
typeCheck gModEnv fileData uState mod@(M.LitMod exprMap modData) = do
    -- get the contraints
    ((tEnv, mTEnv), tCons) <- constrain gModEnv modData Nothing exprMap

    -- unify the types and get the new typemap
    (tVEnv, uVEnv) <- unify uState tCons
    -- substitute to obtain the new type env
    tEnv' <- subTVars tEnv tVEnv uVEnv False
    let modData' = updateModData modData tEnv'

    -- trace ("(TC) " ++ show exprMap) ()
    _ <- trace' [MkSB tEnv'] "Final TypeEnv" $ Right ()

    return $ M.LitMod exprMap modData'

typeCheck gModEnv fileData uState mod@(M.FunctorMod args exprMap modData) = do
    -- get the contraints
    ((tEnv, mTEnv), tCons) <- constrain gModEnv modData (Just args) exprMap

    -- unify the types and get the new typemap`
    (tVEnv, uVEnv) <- unify uState tCons
    -- substitute to obtain the new type env
    tEnv' <- subTVars tEnv tVEnv uVEnv True
    let modData' = updateModData modData tEnv'

    -- functor specific type-checking
    mTEnv' <- subTVars mTEnv tVEnv uVEnv True
    let args' = createFunModArgs args mTEnv'

    return $ M.FunctorMod args' exprMap modData'
  where
    -- create the public module signatures for Functors
    createFunModArgs :: M.FunArgs -> ModTypeEnv -> M.FunArgs
    createFunModArgs args mTEnv = Map.foldrWithKey addArg args mTEnv
      where
        -- add the type for M.v into the args OrdMap
        addArg (E.ModVar m v) t args = OrdMap.update updateModArgs m args
          where
            updateModArgs modMap = Just (Map.insert v t modMap)

-- | Update the module data with the public module signature and internal typemap
-- we create the mod signature by mapping over the idbimap data and looking up each value from the internal typemap
updateModData :: M.ModData -> TypeEnv -> M.ModData
updateModData modData tEnv = modData { M.modTMap = tEnv, M.modSig = sigMap }
  where
    -- build the signature map, taking exported bindings into account
    sigMap = if Set.size (M.modExportSet modData) == 0
        then Map.map (\id -> tEnv Map.! id) $ Bimap.toMap (M.modIdBimap modData) -- export everything
        else Set.foldr updateSig Map.empty (M.modExportSet modData) -- fold over the export set and build the sigMap

    updateSig b sigMap' = Map.insert b t sigMap'
      where
        t = tEnv Map.! ((M.modIdBimap modData) Bimap.! b)

-- | use the TVar map to undate a type enviroment (either TypeEnv or ModTypeEnv) and substitute all TVars
-- Bool argument determinst wheter the checking should allow polymophism and not fully-unify
subTVars :: Show b => Map.Map b E.Type -> TypeVarEnv -> UnitVarEnv -> Bool -> MExcept (Map.Map b E.Type)
subTVars tEnv tVEnv uVEnv allowPoly = DT.mapM (E.mapTypeM updateType) tEnv
  where
    -- try to substitute a tvar if it exists - this will behave differently depending on closed/open modules
    updateType :: E.Type -> MExcept E.Type
    updateType t@(E.TVar i) = processType t $ Map.lookup i tVEnv
    updateType t@(E.TFloat (U.UnitVar uV)) = processType t $ E.TFloat <$> Map.lookup uV uVEnv
    updateType t = return t

    processType :: E.Type -> Maybe E.Type -> MExcept E.Type
    processType t (Just t')   = return t'
    processType t Nothing     = if allowPoly    then return t
                                                else trace' [MkSB tEnv, MkSB tVEnv, MkSB uVEnv] "Poly error" $
                                                        throwError "(TC03) - Type/Unit-variable found in non-polymorphic closed module"

