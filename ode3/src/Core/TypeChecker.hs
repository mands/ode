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

import qualified Core.AST as C
import Utils.Utils

-- TODO - should this be a set, not a list?
-- | Holds the set of type constrains for a var
--newtype TypeMap = TypeMap (Map.Map Int [C.TType])
type TypeMap = Map.Map Int (Set.Set C.TType)
type TypeState = State TypeMap

-- | Add a type contraint for the paritcular var, if it doesn't exsist create a new entry, else cons to the list
addConstraint :: Int -> C.TType -> TypeState ()
addConstraint i t = do
    tM <- get
    let tM' = Map.alter ins' i tM
    put tM'
  where
    ins' Nothing = Just (Set.singleton t)
    ins' (Just s) = Just (Set.insert t s)

-- Easy-HM -
-- run a pass over the model, collecting all elems into a multi-map of possible types
-- then map over the multi-map, for each element unifying the possible types and making sure they match
-- eventually have a single type for each elem, add to the binding sites and all is good
typeCheck :: C.OrdModel Int -> MExcept (C.OrdModel Int)
typeCheck cModel = typedModel
  where
    tM = constrain cModel
    typedModel = unify cModel tM

-- | Create the set of contraints for a particulat type of a var by updateing the typemap
constrain :: C.OrdModel Int -> TypeMap
constrain cModel = tM --DF.foldl consTop Map.empty (C.getOrdSeq cModel)
  where
    tM = execState topM Map.empty -- we only care about the topmap at this stage
    topM = DF.mapM_ consTop (C.getOrdSeq cModel)

    -- functions need to traverse the structure, both passing in and returning expected types that may be used to create constraints
    consTop (C.TopLet i e) = consExpr C.TUnknown e >>= (\eT -> addConstraint i eT)
    consTop (C.TopAbs i arg e) = consExpr C.TUnknown e >>= (\eT -> addConstraint i (C.TArr C.TUnknown eT))

    -- consExpr constrains an expression, passing and returning expected types along with the current typeMap
    -- they don't bother checking that the types are valid, only that the constraints are created
    consExpr inType (C.Var i) = addConstraint i inType >> return inType

    -- literals - we don't care about the input type, instead return the type of the literal
    consExpr _ (C.Lit l) =  return $ getLitType l

    -- TODO - is this right? do we return the arrow type or the inType, add the arrow constraint, as should the initial TopAbs
    -- application, should be an arrow from the type of the expr to the inType
    consExpr inType (C.App i e) = do
        fromType <- consExpr C.TUnknown e
        let arrType = (C.TArr fromType inType)
        addConstraint i arrType
        return inType -- arrType

    -- let, pass intype into e2, i should be type of e1
    consExpr inType (C.Let i e1 e2) = liftM (addConstraint i) (consExpr C.TUnknown e1) >> consExpr inType e2

    -- tuple - FUCK - how does this work, presumably a mapM over the tuple elems, passing the inType to each one?
    consExpr inType (C.Tuple es) = liftM C.TTuple $ DT.mapM consTupleElem es
      where
        -- TODO - do we need to ensure the inType is a TTuple and unpacks it's inTypes to pass down?
        consTupleElem e = consExpr C.TUnknown e

    -- op - we don' care about the inType, override with info regarding the op fucntion,
    consExpr _ (C.Op op e) = consExpr fromT e >> return toT
      where
        (fromT, toT) = case getOpType op of
                        C.TArr f t -> (f, t)

    -- if - we only pass the inType down, don't bother checking that results match up
    consExpr inType (C.If eB eT eF) = consExpr C.TBool eB >> consExpr inType eT >> consExpr inType eF


getLitType :: C.Literal -> C.TType
getLitType l = case l of
    C.Boolean _ -> C.TBool
    C.Num _ -> C.TFloat
    C.NumSeq _ -> C.TFloat

-- TODO - should this be moved into the AST?
-- | Takes an operator and returns the static type of the function
getOpType :: C.Op -> C.TType
getOpType op = case op of
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
    C.Unpack i -> C.TArr (C.TTuple (replicate i C.TUnknown)) C.TUnknown -- TODO - is this right?
    C.Nop -> C.TUnknown -- why is this here??!
  where
    binNum = C.TArr (C.TTuple [C.TFloat, C.TFloat]) C.TFloat
    binRel = C.TArr (C.TTuple [C.TFloat, C.TFloat]) C.TBool
    binLog = C.TArr (C.TTuple [C.TBool, C.TBool]) C.TBool


-- | Unify, if possible, the constraints on an var to generate a single type instance
unify :: C.OrdModel Int -> TypeMap -> MExcept (C.OrdModel Int)
unify cModel tM = trace (show tM) (Right cModel)


-- TODO - not sure if this is fully needed, maybe the unify pass will make these checks redudent
-- | Reconstruct and type-check, final pass that uses unified information to infer the final types for all variables and type checks the resultant model
check :: C.OrdModel Int -> TypeMap -> MExcept (C.OrdModel Int)
check cModel tM = Right cModel
  where
    checkTop tM (C.TopLet i e) = checkExp tM e -- i will be return type of e when e is found
    checkTop tM (C.TopAbs i arg e) = checkExp tM e -- i will be arr from arg type and return type of e when both is found

    -- these functions should all return the type of the expression - it may be TUnknown
    checkExp tM (C.Var i) = tM -- do we pass in the potential type so can addConstraint here
    checkExp tM (C.Lit l) = tM -- need to return the type of the literal
    checkExp tM (C.Let i e1 e2) = tM -- i will be type of e1, will return type of e2
    checkExp tM (C.App i e) = tM -- e needs to be same type as i input, which returns type of i
    checkExp tM (C.Tuple es) = tM -- returns a tuple of the types of the contained elements
    checkExp tM (C.Op op e) = tM -- all e needs to be the same type as the op input, returns op output
    checkExp tM (C.If eB eT eF) = tM -- eB must be bool, eT == eF, return type is same as eT/eF
    checkExp tM _ = undefined
