-----------------------------------------------------------------------------
--
-- Module      :  TypedAST
-- Copyright   :  Copyright (C) 2010 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | A varient of ExecutableAST that uses GADTs to represent a typed intermediate form,
-- | consider converting to AST/CPS form
--
-----------------------------------------------------------------------------

{-# LANGUAGE GADTs, ExistentialQuantification, PatternGuards, KindSignatures #-}

module TypedAST (
    Model, TExp(..), TFun(..), TTyp(..), Type(..), Equal(..), test, BoolExpr(..),
    typeCheck, typeCheck', ATFun(..), ATExp(..),
    typeCheckExp
) where

-- we do type-chcking within the maybe and error monads
import Prelude hiding (GT)
import Data.Maybe
import Control.Monad
import qualified Data.Map as Map

import Utilities
import AST
import qualified ExecutableAST as E

-- basic types should be converted to HOAS/de brujin form to increase type safety
-- functional model, a map of functions that return doubles
type Model = Map.Map Id ATFun

-- set of types available within our object lang
data TTyp a where
    TTDbl   :: TTyp Double
    TTArr   :: TTyp a -> TTyp b -> TTyp (a-> b)

-- functions exist at the top-level - first class only, can't be introducted within expressions
data TFun a where
    TBody   :: TExp a -> TFun a
    TLam    :: Id -> TTyp a -> TFun b -> TFun (a-> b)

-- fully typed expression syntax, represents basic arithmetic expressions
-- kinda useless at present as only one type of expressions allowed, double
-- but need for func calls and returns
data TExp a where
    TDbl    :: Double   -> TExp Double
    TDblOp  :: TExp Double -> NumOp -> TExp Double -> TExp Double
--    TVar    :: Id -> TExp a -- Double
    TVar    :: Id -> TExp Double
    TIf     :: BoolExpr -> TExp a -> TExp a -> TExp a
    TFunCall    :: Id -> [TExp Double] -> TExp Double
    -- "let" introducted to remove need for funcstmt command, whole function is a single recusive expression typed by the return type
    TLet    :: Id -> TTyp a -> TExp a -> TExp b -> TExp b

data BoolExpr   = LogExpr BoolExpr LogOp BoolExpr
                | RelExpr (TExp Double) RelOp (TExp Double) --deriving Show

-- existential container for our typed expressions, a "dynamic typed" component pair of a value and its type
data ATExp = forall a. ATExp (TExp a) (TTyp a)

-- similar for functions
data ATFun = forall a. ATFun (TFun a) (TTyp a)

-- our equality witness - used to prove that two dynamic type vals are equal simply by virtue of existing
-- change to kind function
-- data Equal a b where
data Equal :: * -> *  -> * where
    Eq :: Equal a a

-- test function that compares two object level types for equality
test :: TTyp a -> TTyp b -> Maybe (Equal a b)
test TTDbl TTDbl = return Eq
test (TTArr a b) (TTArr a' b') = do
    Eq <- test a a'
    Eq <- test b b'
    return Eq
-- any other cases, Maybe is a monoid
test _ _ = mzero

-- typing environment, gamma, assoc list
type Gamma = [(Id, ATExp)]

-- a basic typechecker that converts the untyped model AST into the typed functional intermediate AST
-- simple for now, look at "typing dynamic typing" for full method
-- uses maybe monad to progatge type failures
typeCheckExp :: Gamma -> E.Expr -> Maybe ATExp
typeCheckExp gamma (E.Number x) = return $ ATExp (TDbl x) TTDbl
typeCheckExp gamma (E.BinExpr x op y) = do
    -- type check both sides of the expression, remember pattern match silently fails
    -- and returns Nothing in maybe monad
    ATExp x' TTDbl  <- typeCheckExp gamma x
    ATExp y' TTDbl  <- typeCheckExp gamma y
    return $ ATExp (TDblOp x' op y') TTDbl

-- pick previously typechecked Id from typing env gamma, returns maybe so all works out
typeCheckExp gamma (E.ValueRef ident) = lookup ident gamma

typeCheckExp gamma (E.Let ident e b) = do
    ATExp e' eTyp <- typeCheckExp gamma e

    -- forces type to be a double - how can we do pairs??
    Eq <- test eTyp TTDbl
    -- type check the let body using the extend typing env
    ATExp b' bTyp <- typeCheckExp ((ident, ATExp (TVar ident) eTyp):gamma) b
    return $ ATExp (TLet ident eTyp e' b') bTyp

typeCheckExp gamma (E.If cond t e) = do
    -- need to typecheck the bool cond here
    ATExp t' tTyp <- typeCheckExp gamma t
    ATExp e' eTyp <- typeCheckExp gamma e
    -- check both arms have the same type, monadic magic!!
    Eq <- test tTyp eTyp
    -- hack here for bool expr
    let boolHack = RelExpr (TDbl 2.0) GT (TDbl 1.0)
    return $ ATExp (TIf boolHack t' e') tTyp

-- covers all other cases - typechecking fails
typeCheckExp gamma (E.ExtFuncCall ident xs) = do
    return $ ATExp (TDbl 11.0) TTDbl
typeCheckExp gamma (E.IntFuncCall ident xs) = do
    return $ ATExp (TDbl 12.0) TTDbl

-- function type checker
typeCheckFun :: E.Function -> Maybe ATFun
typeCheckFun = typeCheckFun' []

-- typechecks a individual function
typeCheckFun' :: Gamma -> E.Function -> Maybe ATFun
-- we assume for now that all args to functions must be of type TTDbl, adds var to type env and checks rest args
typeCheckFun' gamma (E.Function name (arg:args) body) = do
    ATFun f t <- typeCheckFun' ((arg, ATExp (TVar arg) TTDbl):gamma) (E.Function name args body)
    return $ ATFun (TLam arg TTDbl f) (TTArr TTDbl t)
typeCheckFun' gamma (E.Function name [] body) = do
    ATExp e t <- typeCheckExp gamma body
    return $ ATFun (TBody e) t

-- need a toplevel type checker too, i.e. checks the list of functions
-- converts and typechecks an untyped eAST into the typed AST
-- do we need a typing env for functions ?? - YES ?
typeCheck' :: E.Model -> MExcept [ATFun]
typeCheck' m =
    case (sequence . map typeCheckFun . Map.elems $ m) of
        Just x -> Right x
        Nothing -> Left "Type checking error"

typeCheck :: (Type a) => E.Model -> MExcept [TFun a]
typeCheck m =
    case (sequence . map toTFun . Map.elems $ m) of
        Just x -> Right x
        Nothing -> Left "Type checking error"

-- extraction, used to convert from existential dynamics to typed values, i.e. remove the dynamic tag and allow use in normal haskell
-- this relies on only allowing a set fixed number of types, - look at the eval problem by Oleg
class Type a where
    theType :: TTyp a
instance Type Double where
    theType = TTDbl
instance (Type a, Type b) => Type (a-> b) where
    theType = TTArr theType theType

extractFun :: (Type a) => ATFun -> Maybe (TFun a)
extractFun = extract theType

extract :: TTyp a -> ATFun -> Maybe (TFun a)
extract s (ATFun f t) = do
    Eq <- test t s
    return f

toTFun :: (Type a) => E.Function -> Maybe (TFun a)
toTFun = extractFun <=< typeCheckFun

printTFun :: (Type a) => TFun a -> String
printTFun fun = show fun

-- show instances for our typed \ -calc, boilerplate really but useful as can't derive show for non-Haskell98 ADTs
instance Show (TFun a) where
    showsPrec p (TBody e) =  showsPrec p e
    showsPrec p (TLam i t e) = showParen (p > 0)
        (showString "\\ " . showParen True (showString i . showString "::" . showsPrec 0 t) . showString "->" . showsPrec 0 e)

instance Show (TTyp a) where
    showsPrec _ (TTDbl) = showString "Double"
    showsPrec p (TTArr a b) = showParen (p>5) (showsPrec 6 a.showString "->".showsPrec 5 b)

-- TODO - doesn't respect operator precedance
instance Show (TExp a) where
    showsPrec p (TDbl d) = showsPrec p d
    showsPrec _ (TVar i) = showString i
    showsPrec p (TDblOp a op b) = showsPrec p a.showString (show op). showsPrec p b
    showsPrec p (TIf c t e) = showParen (p>0) (showString "if " . showsPrec 0 c . showString " then " . showsPrec 0 t . showString " else ".showsPrec 0 e)
    showsPrec p (TLet i _ e b) = showParen (p>0) (showString "let " . showString i . showString " = " . showsPrec 0 e . showString " in ". showsPrec 0 b)
    showsPrec p (TFunCall i args) = showString i.showParen (p>0)
        (foldl (\s arg -> s . showsPrec 0 arg) (showString "()") args)

instance Show (BoolExpr) where
    showsPrec p (LogExpr a op b) = showsPrec p a.showString (show op). showsPrec p b
    showsPrec p (RelExpr a op b) = showsPrec p a.showString (show op). showsPrec p b

