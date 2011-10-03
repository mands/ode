-----------------------------------------------------------------------------
--
-- Module      :  Core.Reorderer
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- |Reorderer takes a Core AST and performs a basic reordering of the stamenets
-- all values are defiend before usage
-- At the same time the reorderer tests for cyclic dependenceis between value
-- definitions and in function calls
-- Can issue errors due to user defined model
-- TODO - error monad
-----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures #-}

module Core.Reorderer (
reorder
) where

import Control.Monad.State
import Control.Monad.Error

import qualified Data.Map as Map
import qualified Data.Traversable as DT
import Debug.Trace -- love this shit!
import Control.Applicative
import Data.Graph.Inductive
import qualified Data.Graph.Inductive.Example as E
import Data.Graph.Inductive.Tree -- maybe swtich to PatriciaTree implementation?
import Data.Graph.Inductive.Query.DFS
import Data.Maybe (fromJust)

import qualified Core.AST as C
import Utils.Utils
import qualified Utils.OrdMap as OrdMap

-- define the types we need for our graphs
-- need a topgraph and a topmap - place them both in a state monad and done
type TopGraph = Gr (C.Bind C.SrcId) ()
data TopExpr = LTopLet (C.Bind C.SrcId) | LTopAbs (C.Bind C.SrcId) C.SrcId  deriving Show

--data TopExpr :: *
--  where
--    LTopLet :: (C.Bind C.SrcId) -> TopExpr
--    LTopAbs :: (C.Bind C.SrcId) -> C.SrcId -> TopExpr
--    deriving Show

type ExprGraph = Gr ExprNodeLabel ()
data ExprNodeLabel = ExprNodeLabel (C.Bind C.SrcId) (C.Expr C.SrcId) deriving Show

-- for a top-level id, -> node int, baseexpr, (exprmap - varid -> (node int)), exprgraph
-- we need some maps to hold the non-let body of an expr
type TopMap = Map.Map (C.Bind C.SrcId) TopMapElem
-- a two-stage binding map to map individual binds - need to abstract this datastructure out
type TopBindMap = Map.Map C.SrcId (C.Bind C.SrcId)

data TopMapElem = TopMapElem {  rTopNode :: Int, rTopExpr :: TopExpr, rBaseExpr :: (C.Expr C.SrcId),
                                rExprMap :: Map.Map C.SrcId Int, rExprGraph :: ExprGraph }
                                deriving Show

-- state monad
data ReorderState = ReorderState { rTopGraph :: TopGraph, rTopMap :: TopMap, rTopBindMap :: TopBindMap}
type GraphStateM = StateT ReorderState MExcept
newtype GraphStateMa a = GraphStateMa { runReorder :: StateT ReorderState (MExcept) a }
    deriving (Monad, MonadState ReorderState, MonadError String)

reorder :: C.Module C.SrcId -> MExcept (C.Module C.SrcId)
reorder (C.LitMod exprMap modData) = reorder' exprMap >>= (\exprMap -> (return $ C.LitMod exprMap modData))
reorder (C.FunctorMod args exprMap modData) = reorder' exprMap >>= (\exprMap -> (return $ C.FunctorMod args exprMap modData))

reorder' :: C.ExprMap C.SrcId -> MExcept (C.ExprMap C.SrcId)
reorder' exprMap = do
    -- build the dependency graphs
    (topGraph', topMap') <- procDepGraphs topGraph topMap topBindMap
    -- now we need to sort the graphs and reconstruct the expressions
    exprMap' <- sortGraphs topGraph' topMap'
    return $ trace (show exprMap') exprMap'
  where
    topGraph = createTopGraph exprMap topMap
    topMap = createTopMap exprMap
    topBindMap = createTopBindMap topMap

-- | The main top-level graph
createTopGraph :: C.ExprMap C.SrcId -> TopMap -> TopGraph
createTopGraph exprMap topMap = mkGraph topGraphNodes []
  where
    -- get list of top-graph nodes - need to make sure this matches up with TopMap
    topGraphNodes = convGTop <$> (OrdMap.elems exprMap) -- use applicative style
    -- create a list of the node id (int), and the binding val as the node info
    convGTop (C.TopLet i exp) = (rTopNode $ topMap Map.! i, i)
    convGTop (C.TopAbs i a exp) = (rTopNode $ topMap Map.! i, i)

-- TODO should check here for duplicated ids and throw errors
-- | need to build the TopMap, can use the model to do this
-- fold over the elements, creating a new topmap thru accumulation
createTopMap :: C.ExprMap C.SrcId -> TopMap
createTopMap exprMap = topMap
  where
    (_, topMap) = Map.mapAccum createTopMapElem [1..] (OrdMap.toMap exprMap)

    createTopMapElem (x:xs) (C.TopLet i exp) = (xs, TopMapElem { rTopNode = x, rTopExpr = LTopLet i, rBaseExpr = baseExpr, rExprMap = map, rExprGraph = createExprGraph exp})
      where
        (baseExpr, map) = (createExprMap [1..] exp)

    createTopMapElem (x:xs) (C.TopAbs i a exp) = (xs, TopMapElem { rTopNode = x, rTopExpr = LTopAbs i a, rBaseExpr = baseExpr, rExprMap = map, rExprGraph = createExprGraph exp})
      where
        (baseExpr, map) = (createExprMap [1..] exp)

    createExprMap (x:xs) (C.Let (C.LetBind bs) e1 e2) = (baseExpr, foldl (\map b -> Map.insert b x map) map' bs)
      where
        (baseExpr, map') = (createExprMap xs e2)

    createExprMap (x:xs) baseExpr = (baseExpr, Map.empty)

    -- function to create expression graph
    createExprGraph e = mkGraph (zip [1..] (convGExpr e)) []
    convGExpr (C.Let i e1 e2) = (ExprNodeLabel i e1) : (convGExpr e2)
    convGExpr _ = []


createTopBindMap :: TopMap -> TopBindMap
createTopBindMap topMap = foldl createElem Map.empty (Map.keys topMap)
  where
    createElem map bind@(C.AbsBind b) = Map.insert b bind map
    createElem map bind@(C.LetBind bs) = foldl (\map b -> Map.insert b bind map) map bs


-- TODO - fix use of mapM and assoc to use mapWithKey and lift the monads after
--      - OR, use the graph map/fold functions
-- | New method - map over the elems in the topmap, updating the graphs as required
procDepGraphs :: TopGraph -> TopMap -> TopBindMap -> MExcept (TopGraph, TopMap)
procDepGraphs tg tm tbm = do
    (tm', rs) <- runStateT (runReorder procDepGraphs') (ReorderState tg tm tbm)
    return (rTopGraph rs, Map.fromList tm')
  where
    procDepGraphs' = mapM procTopElem (Map.assocs tm)
    procTopElem (topVar, topElem) = foldM procExprNode (topVar, topElem) (labNodes . rExprGraph $ topElem) >>= procBaseExpr

    -- | Creates dependencies for the let expressions within an expression
    procExprNode :: (C.Bind C.SrcId, TopMapElem) -> LNode ExprNodeLabel -> GraphStateMa (C.Bind C.SrcId, TopMapElem)
    procExprNode (topVar, topElem) (expNode, ExprNodeLabel expVar exp) =
        procExprN topElem (rExprGraph topElem) (Just expNode) exp >>= (\eg -> return (topVar, topElem {rExprGraph = eg}))

    -- | Creates dependencies for the base expression within an expression
    procBaseExpr :: (C.Bind C.SrcId, TopMapElem) -> GraphStateMa (C.Bind C.SrcId, TopMapElem)
    procBaseExpr (topVar, topElem) =
        procExprN topElem (rExprGraph topElem) Nothing (rBaseExpr topElem) >>= (\eg -> return (topVar, topElem {rExprGraph = eg}))


-- | Process an individual expression node within the given expression graph eg
procExprN :: TopMapElem -> ExprGraph -> Maybe Node -> (C.Expr C.SrcId) -> GraphStateMa ExprGraph
procExprN topElem eg mENode exp = procExpr eg exp
  where
    -- NOTE - we have already removed all lets from the expressiong so no defs possible
    -- | Process an individual expression by pattern match over the possibilities
    procExpr :: ExprGraph -> C.Expr C.SrcId -> GraphStateMa ExprGraph
    procExpr eg (C.Var (C.LocalVar useVar)) = updateGraphDep eg useVar -- create a link in the graph from def to use
    procExpr eg (C.App (C.LocalVar useVar) exp) = (updateGraphDep eg useVar) >>= (\eg -> procExpr eg exp)  -- create a link in the graph from def to use

    procExpr eg (C.Op _ exp) = procExpr eg exp
    procExpr eg (C.If exp1 exp2 exp3) = foldM (\eg exp -> procExpr eg exp) eg [exp1,exp2,exp3]

    procExpr eg (C.Tuple exps) = foldM (\eg exp -> procExpr eg exp) eg exps
    procExpr eg _ = return eg -- ignore anything else

    -- helper function
    topBindLookup v m = do
        s <- get
        let bindVar = (rTopBindMap s) Map.! v
        return $ Map.lookup bindVar m

    -- | main function that updates the graph with new edges
    updateGraphDep :: ExprGraph -> C.SrcId -> GraphStateMa ExprGraph
    updateGraphDep eg useVar = do
        -- get the usage's def node from the useVar within the cur elem exprmap
        case (Map.lookup useVar (rExprMap topElem)) of
            -- add a dependency edge from the defUse to the current node
            -- if current expNode exists, i.e. is not a baseExpr
            Just useDefNode -> return $ maybe eg (\eNode -> addDep useDefNode eNode eg) mENode
            Nothing ->
                -- check to see if is the arg within an abs
                case (rTopExpr topElem) of
                    LTopAbs _ arg -> if (arg == useVar) then return eg else checkTopDep
                    -- if not, check the toplevel
                    _ -> checkTopDep
      where
        -- | look in the top level for the expression instead
        checkTopDep = do
            topMap <- liftM rTopMap get
            topNode <- topBindLookup useVar topMap
            case topNode of
                Just useTopElem -> checkTopDep' useTopElem
                -- throwError if lookup fails
                Nothing -> throwError ("(a) Referenced variable " ++ (show useVar) ++ " not found")
          where
            checkTopDep' useTopElem = do
                s <- get
                let tg' = addDep (rTopNode useTopElem) (rTopNode topElem) (rTopGraph s)
                put (s { rTopGraph = tg' }) -- put the updated tg back into StateM
                return eg -- return the unmodified eg

        -- | add a dependency from n1 to n2 within the graph g
        addDep n1 n2 g = insEdge (n1, n2, ()) g


-- | Sorts the top and expressiosn graphs, returning an ordererd map representation of the model
-- also checks for recursive definitions at either level
sortGraphs :: TopGraph -> TopMap -> MExcept (C.ExprMap C.SrcId)
sortGraphs tg tm = do
    sortedTops <- if sccCheck tg then return (topsort' tg)
        -- TODO - need to determine the names of the elements
        else throwError "Found recursive relationship between top-level elements"

    -- need to map over elems in sort top, extract the topmapElem, then sort the exprgraph and regen the top expr
    liftM OrdMap.fromList $ DT.mapM sortExpr sortedTops
  where
    sortExpr topVar = if exprCheck then return (topVar, rE)
        else throwError ("Found recursive relationship between expressions within " ++ (show topVar))
      where
        topElem = (Map.!) tm topVar
        exprCheck = sccCheck (rExprGraph topElem)
        rE = reconExpr (rTopExpr topElem) (topsort' $ rExprGraph topElem) (rBaseExpr topElem)

    -- | function to check for stronglyConnComp within a graph, i.e. any recursive calls
    sccCheck = all (== 1) . map length . scc

    -- | Reconstructs an expression from the list of lets and the baseExpr
    reconExpr :: TopExpr ->  [ExprNodeLabel] -> (C.Expr C.SrcId) -> C.Top C.SrcId
    reconExpr top lets base = case top of
        LTopLet i -> C.TopLet i (reconLets lets)
        LTopAbs i a ->  C.TopAbs i a (reconLets lets)
      where
        reconLets [] = base
        reconLets ((ExprNodeLabel i e):xs) = C.Let i e (reconLets xs)

