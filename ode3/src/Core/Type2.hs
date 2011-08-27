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
-----------------------------------------------------------------------------

module Core.Type2 (
typeCheck
) where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Foldable as DF
import qualified Data.Traversable as DT
import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Text.Printf (printf)
import qualified Core.AST as C
import Utils.Utils
import Utils.MonadSupply


-- | Types
data Type =    -- TUnknown -- TODO - remove
                TVar Int
                -- | TBool
                | TFloat
                | TArr Type Type
                -- | TTuple [Type] -- don't want to allow tuples of tuples
                deriving (Show, Eq, Ord)

--type Type = Primitve TType | Typevar Int

typeCheck :: C.OrdModel Int -> MExcept (C.OrdModel Int)
typeCheck cModel = trace (show typeCons) (return cModel)
--    checkedTypeMap <- check cModel unifiedTypeMap
--    return $ typeModel cModel checkedTypeMap
  where
    typeCons = constrain cModel
    --cModel' = unify cModel typeContraints


type TypeCons = Set.Set (Type, Type)
type TypeConsM = SupplyT Int (State TypeCons)

addConstraint :: Type -> Type -> TypeConsM ()
addConstraint t1 t2 = do
    tS <- lift get
    let tS' = Set.insert (t1, t2) tS
    lift $ put tS'

newTypevar :: TypeConsM Type
newTypevar = liftM TVar supply

constrain :: C.OrdModel Int -> TypeCons
constrain cModel = trace (show tEnv) consSet
  where

    (tEnv, consSet) = runState (evalSupplyT consM [1..]) (Set.empty)

    consM :: TypeConsM (Map.Map Int Type)
    consM = DF.foldlM consTop Map.empty (C.getOrdSeq cModel)

    consTop tEnv (C.TopLet i e) = do
        eT <- consExpr tEnv e
        -- extend and return tEnv
        return $ Map.insert i eT tEnv

    consTop tEnv (C.TopAbs i arg e) = do
        fromT <- newTypevar
        -- extend the tEnv
        let tEnv' = Map.insert arg fromT tEnv
        toT <- consExpr tEnv' e
        -- add a constraint?

        -- extend and return tEnv
        return $ Map.insert i (TArr fromT toT) tEnv'


    -- | map over the expression elements, creating constraints as needed,
    -- TODO - do we need to uniquely refer to each expression within AST?
    consExpr tEnv (C.Var v) = return $ tEnv Map.! v

    -- add the constraint here, for App
    consExpr tEnv (C.App f e) = do
        -- as HOFs not allowed
        -- fT =  consExpr tEnv f
        let fT = tEnv Map.! f
        eT <- consExpr tEnv e
        toT <- newTypevar
        -- add constraint
        addConstraint fT (TArr eT toT)
        return toT

    consExpr tEnv _ = return TFloat


    --consExpr


