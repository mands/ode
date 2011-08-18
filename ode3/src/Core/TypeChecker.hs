-----------------------------------------------------------------------------
--
-- Module      :  Core.TypeChecker
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- |Type checker takes a renamed AST and perfroms type-inference, this is based upon the HM-algorithm, but without
-- let-polymorphism, recursive defintions and so on.
-- Very few primitive types are allowed and user defiend compound types disallowed - hence should be simple
-- Allowed types are, Numerical (Float/Int), Boolean, Function, n-Pair, Ode, and Reaction
-- Can issue errors due to user defined model
-- TODO
-- Use GADTs to enforce terms of correct types (can we do this in earlier expr AST?)
-----------------------------------------------------------------------------

module Core.TypeChecker (
typeCheck
) where

import qualified Data.Map as Map
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

-- | Holds the set of type constrains for a var
--newtype TypeMap = TypeMap (Map.Map Int [C.TType])
-- type TypeMap = Map.Map Int (Set.Set C.TType)
type Type = C.TType Int
type TypeMap = Map.Map Int ([Type])
type TypeState = State TypeMap


-- | Add a type contraint for the paritcular var, if it doesn't exsist create a new entry, else cons to the list
addConstraint :: Int -> Type -> TypeState ()
addConstraint i t = do
    tM <- get
    let tM' = Map.alter ins' i tM
    put tM'
  where
    ins' Nothing = Just [t] -- (Set.singleton t)
    ins' (Just s) = Just (t:s) -- (Set.insert t s)


-- Easy-HM -
-- run a pass over the model, collecting all elems into a multi-map of possible types
-- then map over the multi-map, for each element unifying the possible types and making sure they match
-- eventually have a single type for each elem, add to the binding sites and all is good
typeCheck :: C.OrdModel Int -> MExcept (C.OrdModel Int)
typeCheck cModel = cModel'
  where
    tM = constrain cModel
    tM' = unify cModel tM
    cModel' = check cModel tM'


-- | Create the set of contraints for a particular type of a var by updateing the typemap
constrain :: C.OrdModel Int -> TypeMap
constrain cModel = tM --DF.foldl consTop Map.empty (C.getOrdSeq cModel)
  where
    tM = execState topM Map.empty -- we only care about the topmap at this stage
    topM = DF.mapM_ consTop (C.getOrdSeq cModel)

    -- functions need to traverse the structure, both passing in and returning expected types that may be used to create constraints
    consTop (C.TopLet i e) = consExpr C.TUnknown e >>= (\eT -> addConstraint i eT)
    consTop (C.TopAbs i arg e) = consExpr C.TUnknown e >>= (\eT -> addConstraint i (C.TArr (C.TRef arg) eT))

    -- consExpr constrains an expression, passing and returning expected types along with the current typeMap
    -- they don't bother checking that the types are valid, only that the constraints are created
    -- TODO - is this right - if the inType is unknown then at least save it and return a TRef to the Id
    consExpr inType (C.Var i) = addConstraint i inType >> case inType of
        C.TUnknown -> return (C.TRef i)
        _ -> return inType

    -- literals - we don't care about the input type, instead return the type of the literal
    consExpr _ (C.Lit l) =  return $ getLitType l

    -- TODO - is this right? do we return the arrow type or the inType, add the arrow constraint, as should the initial TopAbs
    -- application, should be an arrow from the type of the expr to the inType

    -- TODO - do we have special behaviour here to handle if inType is unknown - then create a ref to return type of TArr ??
    -- NOT YET - try unify and check first and see if needed
    consExpr inType (C.App i e) = do
        fromType <- consExpr C.TUnknown e
        let arrType = (C.TArr fromType inType)
        addConstraint i arrType
        return inType -- arrType

    -- let, pass intype into e2, i should be type of e1
    consExpr inType (C.Let i e1 e2) = do --liftM (addConstraint i) (consExpr (C.TRef i) e1) >> consExpr inType e2
        t <- consExpr (C.TRef i) e1
        _ <- addConstraint i t
        consExpr inType e2

    -- tuple - if intype is a set list of types then zip with expression and run consExpr,l else treat as unknowns
    consExpr (C.TTuple types) (C.Tuple es) = liftM C.TTuple $ zipWithM consExpr types es
    consExpr inType (C.Tuple es) = liftM C.TTuple $ DT.mapM (consExpr C.TUnknown) es

    -- op - we don' care about the inType, override with info regarding the op fucntion,
    consExpr inType (C.Op op e) = consExpr fromT e >> return toT
      where
        (fromT, toT) = case getOpType op inType of
                        C.TArr f t -> (f, t)

    -- if - we only pass the inType down, don't bother checking that results match up
    consExpr inType (C.If eB eT eF) = consExpr C.TBool eB >> consExpr inType eT >> consExpr inType eF


getLitType :: C.Literal -> Type
getLitType l = case l of
    C.Boolean _ -> C.TBool
    C.Num _ -> C.TFloat
    C.NumSeq _ -> C.TFloat


-- TODO - should this be moved into the AST?
-- | Takes an operator and returns the static type of the function
getOpType :: C.Op -> Type -> Type
getOpType op inType = case op of
    C.Add -> binNum
    C.Sub -> binNum
    C.Mul -> binNum
    C.Div -> binNum
    C.Mod -> binNum
    C.LT -> binRel
    C.LE -> binRel
    C.GT -> binRel
    C.GE -> binRel
    C.EQ -> binRel
    C.NEQ -> binRel
    C.And -> binLog
    C.Or -> binLog
    C.Not -> C.TArr C.TBool C.TBool
    C.Unpack i -> C.TArr (C.TTuple (createUnpackFrom i)) inType -- TODO - is this right?
    C.Nop -> C.TUnknown -- why is this here??!
  where
    binNum = C.TArr (C.TTuple [C.TFloat, C.TFloat]) C.TFloat
    binRel = C.TArr (C.TTuple [C.TFloat, C.TFloat]) C.TBool
    binLog = C.TArr (C.TTuple [C.TBool, C.TBool]) C.TBool
    createUnpackFrom i = hd ++ (inType:tl)
      where
        (hd, tl) = splitAt i . replicate i $ C.TUnknown

-- | Unify, if possible, the constraints on an var to generate a single type instance
unify :: C.OrdModel Int -> TypeMap -> Map.Map Int Type
unify cModel tM = trace (show $ C.getOrdSeq cModel) (trace (show tM) (trace (show tM') tM'))
  where
    (refMap, tM') = Map.mapAccumWithKey unifyTypes Map.empty tM

    -- for each binding, try to unify the list of types - ignore refs for now
    unifyTypes :: TypeMap -> Int -> [Type] -> (TypeMap, Type)
    unifyTypes refTypeMap key types = (refTypeMap, unifyType types)


    unifyType :: [Type] -> Type
    unifyType types = foldl1 unifyType' types
      where
        -- unifyType' takes current type and next type in list and attempts to unify
        unifyType' :: Type -> Type -> Type

        -- concrete types override all
        unifyType' _ C.TFloat = C.TFloat
        unifyType' C.TFloat _ = C.TFloat
        unifyType' _ C.TBool = C.TBool
        unifyType' C.TBool _ = C.TBool

        -- arrow type
        unifyType' oldArr@(C.TArr oldFromT oldToT) newArr@(C.TArr newFromT newToT) =
            C.TArr (unifyType' oldFromT newFromT) (unifyType' oldToT newToT)
        --unifyType' _ newArr@(C.TArr fromT toT) = newArr


        -- tuple type -
        unifyType' (C.TTuple oldTs) (C.TTuple newTs) = C.TTuple $ zipWith unifyType' oldTs' newTs'
          where
            -- TODO - this is a bit hacky
            oldTs' = if length oldTs < length newTs then oldTs ++ replicate (length newTs - length oldTs) C.TUnknown else oldTs
            newTs' = if length newTs < length oldTs then newTs ++ replicate (length oldTs - length newTs) C.TUnknown else newTs

        -- TODO - refs



        -- ignore unknowns
        --unifyType' oldType (C.TRef i) = oldType
        unifyType' C.TUnknown newType = newType
        unifyType' oldType C.TUnknown = oldType

        -- catch all others
        unifyType' _ _ = C.TUnknown



-- TODO - not sure if this is fully needed, maybe the unify pass will make these checks redudent
-- | Reconstruct and type-check, final pass that uses unified information to infer the final types for all variables and type checks the resultant model
check :: C.OrdModel Int -> Map.Map Int Type -> MExcept (C.OrdModel Int)
check cModel tM = cModel'
  where

    -- tM = execState topM Map.empty -- we only care about the topmap at this stage
    cModel' = DT.mapM checkTop (C.getOrdSeq cModel) >>= (\ordSeq -> Right $ C.putOrdSeq (trace (show tM) (trace (show ordSeq) ordSeq)) cModel)

    checkTop :: C.Top Int -> MExcept (C.Top Int)
    -- i will be return type of e when e is found
    checkTop (C.TopLet i e) = checkExp e >> (return $ C.TopLet i e)
    -- i will be arr from arg type and return type of e when both is found
    checkTop (C.TopAbs i arg e) = checkExp e >> (return $ C.TopAbs i arg e)

    -- these functions should all return the type of the expression - it may be TUnknown
    checkExp :: C.Expr Int -> MExcept Type
    checkExp (C.Var i) = return $ Map.findWithDefault C.TUnknown i tM  -- do we pass in the potential type so can addConstraint here

    -- need to return the type of the literal
    checkExp (C.Lit l) = return $ getLitType l

    -- i will be type of e1, will return type of e2
    checkExp (C.Let i e1 e2) = do
        let iType = Map.findWithDefault C.TUnknown i tM
        e1Type <- checkExp e1
        if iType == e1Type
            then checkExp e2
            else throwError (printf "(Let) Variable %u type mismatch - expected %s, found %s" i (show iType) (show e1Type))

    -- e needs to be same type as i input, which returns type of i
    checkExp (C.App i e) = do
        let (C.TArr fromT toT) = tM Map.! i
        eType <- checkExp e
        if fromT == eType then return toT
            else throwError (printf "(App) %i Input type mismatch - expected %s, found %s " i (show fromT) (show eType))

    -- returns a tuple of the types of the contained elements
    checkExp (C.Tuple es) = liftM C.TTuple $ DT.mapM checkExp es
--
--
    -- all e needs to be the same type as the op input, returns op output
    checkExp (C.Op op e) = do
        let (C.TArr fromT toT) = (getOpType op C.TUnknown)
        eType <- checkExp e
        if fromT == eType then return toT
            else throwError (printf "(Op) %s type mismatch - expected %s, found %s" (show op) (show fromT) (show toT))

--
    -- eB must be bool, eT == eF, return type is same as eT/eF
    checkExp (C.If eB eT eF) = do
        eBType <- checkExp eB
        if eBType == C.TBool then return () else
            throwError (printf "(If) Test condition type mismatch - expected Bool, found %s" (show eBType))
        eTType <- checkExp eT
        eFType <- checkExp eF
        if eTType == eFType then return eTType else
            throwError (printf "(If) Branch condition type mismatch - true = %s, false = %s" (show eTType) (show eFType))


    checkExp _ = return C.TUnknown








