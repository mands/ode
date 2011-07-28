-----------------------------------------------------------------------------
--
-- Module      :  Core.AST
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- |A lower-level desuagred AST that represents the mian language features
-- is parameterised by the types of values
-- a reference interpreter exists that may execute on the type-checked Core AST
-- bascially the lambda-calculus style IR
-- should be a functor with fmap defined
-----------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies  #-}

-- {-#LANGUAGE GADTs, EmptyDataDecls, KindSignatures #-}

module Core.AST (
Id,
ModelMap(..), Model, ListModel, OrdModel, getOrdMap, getOrdSeq,
Top(..), Expr(..), Op(..), Literal(..),
) where

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Sequence as Seq
import qualified Data.List as List
import qualified Data.Foldable as DF
import qualified Data.Traversable as DT
import Data.Maybe (fromJust, isJust)
import Utils.Utils

-- |Identifier - basicially RdrName - needs to become parameterised
type Id = String
-- |NewIdentifier - holds both a (parameterised) identifier and a string that represetns the (closest) original/source variable
-- TODO - change to newtype
data NewId a = NewId a String


-- |Top level Core model
-- need to make sure this is an ordered map so we keep the evaluation order correct
-- TODO - change the rep to an assoc list for now
--   later conv to newtype of hash/int map combined with a sequence
-- maybe use number and Id to index/key
type Model b =  Map.Map b (Top b)

type ListModel b = [(b, Top b)]

--data OrdModel b = OrdModel { ordMap :: IntMap.IntMap b, ordSeq :: Seq.Seq b }
data IOrdModel b = IOrdModel { ordMap :: Map.Map b (Top b), ordSeq :: Seq.Seq b } deriving Show

-- Newtype test - is it needed in this case - type classes should be enough with escape hatch if neccessary
newtype OrdModel b = OrdModel { getOrdModel :: IOrdModel b } deriving Show
getOrdMap :: OrdModel b -> Map.Map b (Top b)
getOrdMap om = ordMap . getOrdModel $ om

getOrdSeq :: OrdModel b -> Seq.Seq b
getOrdSeq om = ordSeq . getOrdModel $ om

-- |standard typeclass for a top-level model map structure
-- most functions cribbed from Data.Map
-- add more as needed, only basic essential funcs here, rest acessible via escape hatch to direct map
-- need to make instance of Traversable and Foldable
class ModelMap a b | a -> b where
    -- |Perform a direct lookup for given binding, throw error if not found
    (!) :: (Ord b) => a -> b -> Top b
    -- |return the topval within a Maybe for the given binding
    lookup :: (Ord b) => b -> a -> Maybe (Top b) -- keep same order args as Data.Map (even tho wrong)
    -- |Check if the binding is within the model
    member :: (Ord b) => b -> a -> Bool
    -- |Create an empty model
    empty :: a
    -- |Create a new model from a single top-binding
    singleton :: b -> (Top b) -> a
    -- |Insert the binding into the model, taking into account the insertion order
    insert :: (Ord b) => b -> (Top b) -> a -> a
    -- |Delete the binding from the model
    delete :: (Ord b) => b -> a -> a
    -- |if elem exists, run the function over the topval, if returns Just then update, if None then delete
    update :: (Ord b) => (Top b -> Maybe (Top b)) -> b -> a -> a
    -- |Map over elements in order independent manner,
    -- does not allow for chaning the types of the binding
    -- should this then run fmap over the internal elems?
    map :: (Top b -> Top b) -> a -> a
    -- |Fold over the elements, taking into account the ordering
    fold :: (Ord b) => (Top b -> c -> c) -> c -> a -> c
    -- |The top-level elements in the model
    elems :: (Ord b) => a -> [Top b]
    -- |The top-level bindings in the model
    keys :: a -> [b]
    -- |An association-list repsentation of the model
    toList :: (Ord b) => a -> [(b, Top b)] -- this may be ordered depending on the type of model
    -- |Convert an assoication list binding/top-elem into a model
    fromList :: Ord b => [(b, Top b)] -> a -- this initial ordering may be retained

-- |Basic wrapper around Data.Map
instance ModelMap (Model b) b where
    (!) m b = m Map.! b
    lookup b m = Map.lookup b m
    member b m = Map.member b m
    empty = Map.empty
    singleton b v = Map.singleton b v
    insert b v m = Map.insert b v m
    delete b m = Map.delete b m
    update f b m = Map.update f b m
    map f m = Map.map f m -- Map.map (\v -> fmap f v) m
    fold f z m = Map.fold f z m
    elems m = Map.elems m
    keys m = Map.keys m
    toList m = Map.toList m
    fromList xs = Map.fromList xs

-- |this model rep holds insertion order correctly but is inefficent (O(N))on lookups
-- the order is stored from left-to-right within the list, with ealier ordered elems coming first within the list
instance ModelMap (ListModel b) b where
    (!) m b = fromJust $ List.lookup b m
    lookup b m = List.lookup b m
    member b m = isJust $ List.lookup b m
    empty = []
    singleton b v = (b,v) : []

   -- if already exsists use existing insertion order, else add at the tail of list
    insert b v m = maybe insert' update' mInd
      where
        update' i = let (hd', tl') = splitAt i m
                    in hd' ++ ((b, v) : (tail tl'))
        insert' = m ++ [(b,v)]
        mInd = List.elemIndex b (keys m)

    delete b m = maybe m delete' mInd
      where
        mInd = List.elemIndex b (keys m)
        delete' i = let (hd', tl') = splitAt i m
                    in hd' ++ (tail tl')

    update f b m = if member b m then update' (f (m!b)) else m
      where
        update' (Just v) = insert b v m
        update' Nothing = delete b m

    map f m = List.map (\(b, v) -> (b, f v)) m
    fold f z m = List.foldl (\z (b, v) -> f v z) z m -- do a left fold as in order
    elems m = List.map snd m
    keys m = List.map fst m
    toList = id
    fromList = id

-- |Optimised OrdModel that uses both a map and a seq to hold the ordering of elements
instance ModelMap (OrdModel b) b where
    (!) m b = (getOrdMap m) Map.! b
    lookup b m = Map.lookup b (getOrdMap m)
    member b m = Map.member b (getOrdMap m)
    empty = OrdModel $ IOrdModel Map.empty Seq.empty
    singleton b v = OrdModel $ IOrdModel (Map.singleton b v) (Seq.singleton b)
    insert b v m = OrdModel $ IOrdModel map' seq'
      where
        map' = Map.insert b v (getOrdMap m)
        seq' = if member b m -- if already in model
            then Seq.update ind b (getOrdSeq m)  -- then update in cur pos
            else (getOrdSeq m) Seq.|> b -- else append to end of seq
        ind = fromJust $ Seq.elemIndexL b (getOrdSeq m)

    delete b m = OrdModel $ IOrdModel map' seq'
      where
        map' = Map.delete b (getOrdMap m)
        seq' = if member b m
            then hd' Seq.>< (Seq.drop 1 tl')
            else getOrdSeq m
        ind = fromJust $ Seq.elemIndexL b (getOrdSeq m)
        (hd', tl') = Seq.splitAt ind (getOrdSeq m)

    update f b m = if member b m then update' (f (m!b)) else m
      where
        update' (Just v) = insert b v m
        update' Nothing = delete b m

    map f m = OrdModel $ IOrdModel map' (getOrdSeq m)
      where
        map' = Map.map f (getOrdMap m)

    fold f z m = DF.foldl' f' z (getOrdSeq m)
      where
        f' z' b = f (m!b) z'

    elems m = List.map snd (toList m)

    keys m = DF.toList (getOrdSeq m)

    toList m = List.map (\b -> (b, m!b)) (keys m)

    fromList xs = DF.foldl' (\m (b, v) -> insert b v m) empty xs

-- |Basic instances
instance DF.Foldable OrdModel where
    foldr f z m = DF.foldr f z (getOrdSeq m)

--    foldr f z m = DF.foldr f' z (getOrdSeq m)
--      where
--        f' :: Top a -> b -> b
--        f' b z' = f ((getOrdMap m) Map.! b) z'

-- |Basic \-Calc
-- not used - just for reference
-- Positives
-- * Simple, and unified expression and top-level
-- * known basic optimisations, is a known quantity with huge amount of literature
-- * beautiful and expressive
-- Negatives
-- * Allows only single argument for Abstractions, thus result to currying, pair-consing, or tuples for fixed arity funcs
--   (and lists for mult-values, unknown arity expressions)
--   Thus have discord between input params, that are curried, and output params, that would be tuples for mult-vals
--   Currying requires closure creation and optimisation, expensive and complicated while are not expressed in front-end language
--   (tho may make type-cechking easier)
-- * It allows HOF, closures and anonymous functions, the increased flexability/power of these are not required/supported in the front-end
--   and add additional complexity and flexability
-- * It makes it harder to see actual structure of program, need to apply the AST to tell what it's doing,
--   for instance is Apply to a anon func, a local-let func, or a top-level let func?
-- * People modify/extend the \-calc all the time for specific use cases - that is novel in itself and worth it

data LTop       = LTopLet Id LExpr

data LExpr      = LVar Id
                | LLit Literal
                | LAbs Id LExpr
                | LApp LExpr LExpr
                | LLet Id LExpr LExpr
                | LPair LExpr LExpr
                | LOp Op
                deriving Show

-- |Main model elements - maybe move these into a Map indexed by Id
data Top b  = TopLet b (Expr b)    -- binding, expr
            | TopAbs b b (Expr b) -- binding, abs name, expr
            deriving Show

-- | Main body of a \c-calc expression
-- is parameterised by the binding type b - used for RdrNames/Ids, Uniques, Uniques+Types
-- is restricted from default \-calc to disallow nested functions, HOF, currying, anony functions and more
-- is extended from default \-calc to support literals (inc. numbers, bools), pairs, and built-in operators (effectily Vars)
-- disabling currying means that all functions take only a single parameter, and evalute to an expression,
-- thus to pass/return multiple values simple used pair consing
-- TODO - should this be a GADT??
data Expr b = Var b                    -- a reference to any let-defined expressions
                                        -- could potentially ref to a top-level abs but unlikely, would be optimised

            | Lit Literal               -- basic built-in constant literals

            | App b (Expr b)           -- name of top-level func, expression to apply
                                        -- by using an Id instead of Expr we effecitively disallow anon-funcs and HOF, we can only
                                        -- call top-level variables that may then be applied

            | Let b (Expr b) (Expr b)  -- basic let within sub-expression

            | Op Op (Expr b)    -- is basically identical to App - however is used to refer to built-in/run-time functions
                                -- we could but don't curry as would like to apply same optimsations to both sys/user functions
                                -- instead pass pair-cons of expressions

            | If (Expr b) (Expr b) (Expr b) -- standard if construct, used to apply piecewise/case constructs

            | Pair (Expr b) (Expr b)    -- cons expressions into a Pair/Product construct, can nest abritarily to create n-Tuples
                                        -- how do we unpack??
                                        -- can use pattern matching in the front-end/Ode lang, convert it to list of (top-)lets using
                                        -- fst/snd Op functions over the recustive Pair definition
                                        -- how do we force only consing of pairs into n-Tuples?
                                            -- use a list? - but then could have lists-of-lists
                                            -- create own data-type
                                            -- don't worry about it and maybe change later?

            | Tuple [Expr b]            -- just a test, could be used instead of pairs, makes some ops easier

            -- now add the simulation stuff!
            deriving Show

-- |Atomic, core values, will eventually become atomic args during ANF conversion
data Literal =  Num Double | NumSeq [Double] | Boolean Bool
                deriving Show

-- |built-in operators - basically any operators that may be expressed directly as hardware instructions or sys/built-ins
data Op = Add | Sub | Mul | Div | Mod
        | LT | LE | GT | GE | EQ | NEQ
        | And | Or | Not
        | Fst | Snd | Unpack Int -- used for unpacking values from Pairs/Tuples
        | Nop -- not used?
        deriving Show


-- create a few typeclasss instances

-- |Standard functor defintion, could be derived automatically but still...
-- only applicable for the binding parameter, so maybe useless
-- could be used to determine bindings/fv, etc.
instance Functor Top where
    fmap f (TopLet x expr) = TopLet (f x) (fmap f expr)
    fmap f (TopAbs x arg expr) = TopAbs (f x) (f arg) (fmap f expr)

instance Functor Expr where
    fmap f (Var a) = Var (f a)
    fmap f (Lit a) = Lit a
    fmap f (App x e) = App (f x) (fmap f e)
    fmap f (Let x e1 e2) = Let (f x) (fmap f e1) (fmap f e2)
    fmap f (Op op e) = Op op (fmap f e)
    fmap f (If e1 e2 e3) = If (fmap f e1) (fmap f e2) (fmap f e3)
    fmap f (Pair e1 e2) = Pair (fmap f e1) (fmap f e2)
    fmap f (Tuple es) = Tuple (List.map (\e -> fmap f e) es)

instance PrettyPrint Op where
    prettyPrint Add = "+"
    prettyPrint Sub = "-"
    prettyPrint Mul = "*"
    prettyPrint Div = "/"
    prettyPrint Mod = "%"

    prettyPrint Core.AST.LT = "<"
    prettyPrint LE = "<="
    prettyPrint Core.AST.GT = ">"
    prettyPrint GE = ">="
    prettyPrint Core.AST.EQ = "=="
    prettyPrint NEQ = "!="

    prettyPrint And = "&&"
    prettyPrint Or = "!!"
    prettyPrint Not = "!"

    prettyPrint Fst = ".1"
    prettyPrint Snd = ".2"
    prettyPrint (Unpack a) = "!!" ++ show a

    prettyPrint Nop = "NOP"

instance PrettyPrint Literal where
    prettyPrint (Num a) = show a
    prettyPrint (NumSeq a) = show a
    prettyPrint (Boolean a) = show a

instance PrettyPrint Id where
    prettyPrint id = show id

instance (Show a) => PrettyPrint (Top a) where
    prettyPrint (TopLet x expr) = "let " ++ show x ++ " = " ++ prettyPrint expr
    prettyPrint (TopAbs x arg expr) = show x ++ " = \\" ++ show arg ++ ". " ++ prettyPrint expr

instance (Show a) => PrettyPrint (Expr a) where
    prettyPrint (Var x) = show x
    prettyPrint (Lit x) = case x of
                            Num y -> show y
                            NumSeq ys -> show ys
                            Boolean b -> show b

    prettyPrint (App x expr) = show x ++ prettyPrint expr

    prettyPrint (Let x bindExpr inExpr) = "let " ++ show x ++ " = " ++ prettyPrint bindExpr ++ " in \n" ++ prettyPrint inExpr

    prettyPrint (Op op expr) = prettyPrint op ++ prettyPrint expr
    prettyPrint (If ifExpr tExpr fExpr) = "if (" ++ prettyPrint ifExpr ++ ") then " ++ prettyPrint tExpr ++ " else " ++
                                            prettyPrint fExpr

    prettyPrint (Pair e1 e2) = "(" ++ prettyPrint e1 ++ ", " ++ prettyPrint e2 ++ ")"

    prettyPrint (Tuple es) = "(" ++ tuples ++ ")"
      where
        tuples = concat . Prelude.map (\e -> (prettyPrint e) ++ ", ") $ es

--type Model b =  Map Id (Top b)
instance (Show a) => PrettyPrint (Model a) where
    prettyPrint model = unlines . Prelude.map (\e -> prettyPrint e) $ Map.elems model



