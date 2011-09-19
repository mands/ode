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

{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FunctionalDependencies  #-}
{-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures #-}

module Core.AST (
Id, Bind(..), Type(..), TypedId(..), travTypes,
ModelMap(..), Model, ListModel, OrdModel, getOrdMap, getOrdSeq, putOrdMap, putOrdSeq, getTopBinding,
Top(..), Expr(..), Op(..), Literal(..),
) where

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Sequence as Seq
import qualified Data.List as List
import qualified Data.Foldable as DF
import qualified Data.Traversable as DT
import Data.Functor
import Data.Maybe (fromJust, isJust)
import Utils.Utils

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

-- TODO - could we use the Bind type to unify both b and [b], or use GADTs and type-classes for extra type-safety
-- |Main model elements - maybe move these into a Map indexed by Id
data Top b :: * where
    TopLet :: (Bind b) -> (Expr b) -> Top b    -- binding, expr
    TopAbs :: (Bind b) -> b -> (Expr b) -> Top b -- binding, abs name, expr
    deriving Show

-- | Main body of a \c-calc expression
-- is parameterised by the binding type b - used for RdrNames/Ids, Uniques, Uniques+Types
-- is restricted from default \-calc to disallow nested functions, HOF, currying, anony functions and more
-- is extended from default \-calc to support literals (inc. numbers, bools), pairs, and built-in operators (effectily Vars)
-- disabling currying means that all functions take only a single parameter, and evalute to an expression,
-- thus to pass/return multiple values simple used pair consing
-- TODO - should this be a GADT??, should "b" be an instance of Ord
data Expr b = Var b                    -- a reference to any let-defined expressions
                                        -- could potentially ref to a top-level abs but unlikely, would be optimised

            | Lit Literal               -- basic built-in constant literals

            | App b (Expr b)           -- name of top-level func, expression to apply
                                        -- by using an Id instead of Expr we effecitively disallow anon-funcs and HOF, we can only
                                        -- call top-level variables that may then be applied

            | Let (Bind b) (Expr b) (Expr b)  -- basic let within sub-expression
                                        -- test to try multi-lets within an expressino - handles unpacking with context

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
        -- | Fst | Snd | Unpack Int -- used for unpacking values from Pairs/Tuples
        | Nop -- not used?
        deriving Show

-- | Identifier - basicially RdrName - needs to become parameterised
type Id = String
-- TODO - change to newtype
-- | NewIdentifier - holds both a (parameterised) identifier and a string that represetns the (closest) original/source variable and line num
data NewId a = NewId a String Int


-- | Types
data Type :: * where
    -- TUnknown :: Type -- TODO - remove
    TVar :: Int -> Type
    TBool :: Type
    TFloat :: Type
    TArr :: Type -> Type -> Type
    TTuple :: [Type] -> Type -- don't want to allow tuples of tuples
    deriving (Show, Eq, Ord)

travTypes :: Type -> (Type -> Type) -> Type
travTypes (TArr fromT toT) f = TArr (travTypes fromT f) (travTypes toT f)
travTypes (TTuple ts) f = TTuple $ map f ts
travTypes t f = f t


data TypedId = TypedId Int Type
    deriving (Show, Eq, Ord)

-- could entually optimise this and make more type-safe but this works for now
data Bind b = AbsBind b | LetBind [b]
    deriving (Show, Eq, Ord)

-- TODO -- maybe use number and Id to index/key
-- | Top level Core model
-- need to make sure this is an ordered map so we keep the evaluation order correct
type Model b =  Map.Map (Bind b) (Top b)

type ListModel b = [(Bind b, Top b)]

--data OrdModel b = OrdModel { ordMap :: IntMap.IntMap b, ordSeq :: Seq.Seq b }
data IOrdModel b = IOrdModel { ordMap :: Map.Map (Bind b) Int, ordSeq :: Seq.Seq (Top b) } deriving Show

-- Newtype test - is it needed in this case - type classes should be enough with escape hatch if neccessary
newtype OrdModel b = OrdModel { getOrdModel :: IOrdModel b } deriving Show
getOrdMap :: OrdModel b -> Map.Map (Bind b) Int
getOrdMap om = ordMap . getOrdModel $ om

putOrdMap :: Map.Map (Bind b) Int -> OrdModel b -> OrdModel b
putOrdMap map m = OrdModel $ IOrdModel map (getOrdSeq m)

getOrdSeq :: OrdModel b -> Seq.Seq (Top b)
getOrdSeq om = ordSeq . getOrdModel $ om

putOrdSeq :: Seq.Seq (Top b) -> OrdModel b -> OrdModel b
putOrdSeq seq m = OrdModel $ IOrdModel (getOrdMap m) seq

getTopBinding :: Top b -> (Bind b, Top b)
getTopBinding t@(TopLet b _) = (b,t)
-- TODO - should abs be wrapped in a bind?
getTopBinding t@(TopAbs b _ _) = (b,t)


-- |standard typeclass for a top-level model map structure
-- most functions cribbed from Data.Map
-- add more as needed, only basic essential funcs here, rest acessible via escape hatch to direct map
-- need to make instance of Traversable and Foldable
class ModelMap a b | a -> b where -- a is the Data Strcuture, b is the Bind Type Variable
    -- | Perform a direct lookup for given binding, throw error if not found
    (!) :: (Ord b) => a -> (Bind b) -> Top b
    -- | return the topval within a Maybe for the given binding
    lookup :: (Ord b) => (Bind b) -> a -> Maybe (Top b) -- keep same order args as Data.Map (even tho wrong)
    -- | Check if the binding is within the model
    member :: (Ord b) => (Bind b) -> a -> Bool
    -- | Create an empty model
    empty :: a
    -- | Create a new model from a single top-binding
    singleton :: (Bind b) -> (Top b) -> a
    -- | Insert the binding into the model, taking into account the insertion order
    insert :: (Ord b) => (Bind b) -> (Top b) -> a -> a
    -- | Delete the binding from the model
    delete :: (Ord b) => (Bind b) -> a -> a
    -- | if elem exists, run the function over the topval, if returns Just then update, if None then delete
    update :: (Ord b) => (Top b -> Maybe (Top b)) -> (Bind b) -> a -> a
    -- | Map over elements in order independent manner,
    -- does not allow for chaning the types of the binding
    -- should this then run fmap over the internal elems?
    -- provided by instances of Functor and Traversable
    --map :: (Top b -> Top b) -> a -> a
    -- | Fold over the elements, taking into account the ordering
    --fold :: (Ord b) => (Top b -> c -> c) -> c -> a -> c
    -- | The top-level elements in the model
    elems :: (Ord b) => a -> [Top b]
    -- | The top-level bindings in the model
    keys  :: (Ord b) => a -> [Bind b]
    -- | An association-list repsentation of the model
    toList :: (Ord b) => a -> [(Bind b, Top b)] -- this may be ordered depending on the type of model
    -- | Convert an assoication list binding/top-elem into a model
    fromList :: Ord b => [(Bind b, Top b)] -> a -- this initial ordering may be retained


-- create a few typeclasss instances

-- | Basic wrapper around Data.Map
instance ModelMap (Model b) b where
    (!) m b = m Map.! b
    lookup b m = Map.lookup b m
    member b m = Map.member b m
    empty = Map.empty
    singleton b v = Map.singleton b v
    insert b v m = Map.insert b v m
    delete b m = Map.delete b m
    update f b m = Map.update f b m
    --map f m = Map.map f m -- Map.map (\v -> fmap f v) m
    --fold f z m = Map.fold f z m
    elems m = Map.elems m
    keys m = Map.keys m
    toList m = Map.toList m
    fromList xs = Map.fromList xs

-- | this model rep holds insertion order correctly but is inefficent (O(N))on lookups
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

    --map f m = List.map (\(b, v) -> (b, f v)) m
    --fold f z m = List.foldl (\z (b, v) -> f v z) z m -- do a left fold as in order
    elems m = List.map snd m
    keys m = List.map fst m
    toList = id
    fromList = id

-- | Optimised OrdModel that uses both a map and a seq to hold the ordering of elements
instance ModelMap (OrdModel b) b where
    (!) m b = Seq.index (getOrdSeq m) ((getOrdMap m) Map.! b)
    lookup b m = Map.lookup b (getOrdMap m) >>= (\i -> return $ Seq.index (getOrdSeq m) i)
    member b m = Map.member b (getOrdMap m)
    empty = OrdModel $ IOrdModel Map.empty Seq.empty
    singleton b v = OrdModel $ IOrdModel (Map.singleton b 0) (Seq.singleton v)
    insert b v m = OrdModel $ IOrdModel map' seq'
      where
        map' = if member b m then map else Map.insert b (Seq.length seq) map
        seq' = if member b m -- if already in model
            then Seq.update (map Map.! b) v seq  -- then update in cur pos
            else seq Seq.|> v -- else append to end of seq
        map = getOrdMap m
        seq = getOrdSeq m

    delete b m = OrdModel $ IOrdModel map' seq'
      where
        map' = Map.delete b map
        seq' = if member b m
            then hd' Seq.>< (Seq.drop 1 tl')
            else seq
        --ind =  -- fromJust $ Seq.elemIndexL b (getOrdSeq m)
        (hd', tl') = Seq.splitAt (map Map.! b) seq
        map = getOrdMap m
        seq = getOrdSeq m


    update f b m = if member b m then update' (f (m!b)) else m
      where
        update' (Just v) = insert b v m
        update' Nothing = delete b m

--    map f m = OrdModel $ IOrdModel map' (getOrdSeq m)
--      where
--        map' = Map.map f (getOrdMap m)
--
--    fold f z m = DF.foldl' f' z (getOrdSeq m)
--      where
--        f' z' b = f (m!b) z'

    elems m = DF.toList $ getOrdSeq m

    keys m = List.map fst (toList m)

    toList m = DF.toList . fmap getTopBinding $ getOrdSeq m

    --List.map (\b -> (b, m!b)) (keys m)

    fromList xs = DF.foldl' (\m (b, v) -> insert b v m) empty xs

-- |Basic instances

-- this can't be done as the contained object within OrdModel is not the same as that within the sequence (i.e. b vs Top b)
-- instead need to get the seq directly to fold over
--instance DF.Foldable OrdModel where
    --foldr :: (a -> b -> b) -> b -> t a -> b
--    foldr f z m = DF.foldr f z (getOrdSeq m)


-- |Standard functor defintion, could be derived automatically but still...
-- only applicable for the binding parameter, so maybe useless
-- could be used to determine bindings/fv, etc.

-- Again can't do for same reason as above, model holds b, seq holds (Top b) - not compatible
--instance Functor OrdModel where
    -- need to fmap over the sequence, then recreate the orig structure using insert with a fold
--    fmap f model = model
--      where
--        newSeq = fmap f (getOrdSeq model)
instance Functor Bind where
    fmap f (AbsBind b) = AbsBind $ f b
    fmap f (LetBind b) = LetBind $ map f b

instance Functor Top where
    fmap f (TopLet b expr) = TopLet (fmap f b) (fmap f expr)
    fmap f (TopAbs b arg expr) = TopAbs (fmap f b) (f arg) (fmap f expr)

instance Functor Expr where
    fmap f (Var a) = Var (f a)
    fmap f (Lit a) = Lit a
    fmap f (App x e) = App (f x) (fmap f e)
    fmap f (Let b e1 e2) = Let (fmap f b) (fmap f e1) (fmap f e2)
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

--    prettyPrint Fst = ".1"
--    prettyPrint Snd = ".2"
--    prettyPrint (Unpack a) = "!!" ++ show a

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



