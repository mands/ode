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
-- TODO
-- need to reconstruct the expressions again after sorting
-- what data structure to hold insertion-ordered map
--   for now use assoc-list
--   later conv to newtype of hash/int map combined with a sequence
-----------------------------------------------------------------------------

module Core.Reorderer (
reorder
) where

import Control.Monad.State
import qualified Data.Map as Map
import Debug.Trace -- love this shit!
import Control.Applicative
import Data.Graph.Inductive
import qualified Data.Graph.Inductive.Example as E
import Data.Graph.Inductive.Tree -- maybe swtich to PatriciaTree implementation?
import Data.Graph.Inductive.Query.DFS
import Data.Maybe (fromJust)
import qualified Core.AST as C
import Utils.Utils

-- define the types we need for our graphs
-- need a topgraph and a topmap - place them both in a state monad and done
type TopGraph = Gr C.Id ()
--type TopGraph = Gr TopNodeLabel ()
data TopExpr = LTopLet C.Id | LTopAbs C.Id C.Id deriving Show

type ExprGraph = Gr ExprNodeLabel ()
data ExprNodeLabel = ExprNodeLabel C.Id (C.Expr C.Id) deriving Show

-- for a top-level id, -> node int, baseexpr, (exprmap - varid -> (node int)), exprgraph
-- we need some maps to hold the non-let body of an expr
type TopMap = Map.Map C.Id TopMapElem
data TopMapElem = TopMapElem {  rTopNode :: Int, rTopExpr :: TopExpr, rBaseExpr :: (C.Expr C.Id),
                                rExprMap :: Map.Map C.Id Int, rExprGraph :: ExprGraph }
                                deriving Show

-- state monad
data ReorderState = ReorderState { rTopGraph :: TopGraph, rTopMap :: TopMap}
type GraphStateM = State ReorderState

reorder :: C.Model C.Id -> MExcept (C.Model C.Id)
reorder cModel = (trace (show topGraph ++ "\n") (trace (Map.showTree topMap) (trace (show topGraph' ++ "\n") (trace (Map.showTree topMap') Right res))))
  where

    res = if (length sortModel == 0) then cModel else cModel
    -- now we need to sort the graphs and reconstruct the expressions
    sortModel = sortGraphs topGraph' topMap'

    -- build the dependency graphs
    (topGraph', topMap') = procDepGraphs topGraph topMap
    topGraph = createTopGraph cModel topMap
    topMap = createTopMap cModel


-- |The main top-level graph
createTopGraph :: C.Model C.Id -> TopMap -> TopGraph
createTopGraph cModel topMap = mkGraph topGraphNodes []
  where
    -- get list of top-graph nodes - need to make sure this matches up with TopMap
    topGraphNodes = convGTop <$> (Map.elems cModel) -- use applicative style
    convGTop (C.TopLet i exp) = (rTopNode $ (Map.!) topMap i, i)
    convGTop (C.TopAbs i a exp) = (rTopNode $ (Map.!) topMap i, i)


-- |need to build the TopMap, can use the model to do this
-- fold over the elements, creating a new topmap thru accumulation
-- TODO - cleanup
-- TODO should check here for duplicated ids and throw errors
createTopMap :: C.Model C.Id -> TopMap
createTopMap cModel = topMap
  where
    (_, topMap) = Map.mapAccum createTopMapElem [1..] cModel


    createTopMapElem (x:xs) (C.TopLet i exp) = (xs, TopMapElem { rTopNode = x, rTopExpr = LTopLet i, rBaseExpr = baseExpr, rExprMap = map, rExprGraph = createExprGraph exp})
      where
        (baseExpr, map) = (createExprMap [1..] exp)

    createTopMapElem (x:xs) (C.TopAbs i a exp) = (xs, TopMapElem { rTopNode = x, rTopExpr = LTopAbs i a, rBaseExpr = baseExpr, rExprMap = map, rExprGraph = createExprGraph exp})
      where
        (baseExpr, map) = (createExprMap [1..] exp)

    createExprMap (x:xs) (C.Let i e1 e2) = (baseExpr, Map.insert i x map')
      where
        (baseExpr, map') = (createExprMap xs e2)
    createExprMap (x:xs) baseExpr = (baseExpr, Map.empty)

    -- function to create expression graph
    createExprGraph e = mkGraph (zip [1..] (convGExpr e)) []
    convGExpr (C.Let i e1 e2) = (ExprNodeLabel i e1) : (convGExpr e2)
    convGExpr _ = []

-- |New method - map over the elems in the topmap, updating the graphs as required
-- TODO - fix use of mapM and assoc to use mapWithKey and lift the monads after
procDepGraphs :: TopGraph -> TopMap -> (TopGraph, TopMap)
procDepGraphs tg tm = (rTopGraph rs, Map.fromList tm')
  where
    (tm', rs) = runState procDepGraphs' (ReorderState tg tm)
    procDepGraphs' = mapM procTopElem (Map.assocs tm)
    procTopElem (topVar, topElem) = foldM procExprNode (topVar, topElem) (labNodes . rExprGraph $ topElem)

-- |Process an individual expression node within the given expression graph eg
procExprNode :: (C.Id, TopMapElem) -> LNode ExprNodeLabel -> GraphStateM (C.Id, TopMapElem)
procExprNode (topVar, topElem) (expNode, ExprNodeLabel expVar exp) =
    procExpr (rExprGraph topElem) exp >>= (\eg -> return (topVar, topElem {rExprGraph = eg}))

  where
    -- |Process an individual expression by pattern match over the possibilities
    -- note - we have already removed all lets from the expressiong so no defs possible
    procExpr :: ExprGraph -> C.Expr C.Id -> GraphStateM ExprGraph
    procExpr eg (C.Var useVar) = updateGraphDep eg useVar -- create a link in the graph from def to use
    procExpr eg (C.App useVar exp) = (updateGraphDep eg useVar) >>= (\eg -> procExpr eg exp)  -- create a link in the graph from def to use

    procExpr eg (C.Op _ exp) = procExpr eg exp
    procExpr eg (C.If exp1 exp2 exp3) = foldM (\eg exp -> procExpr eg exp) eg [exp1,exp2,exp3]

    procExpr eg (C.Tuple exps) = foldM (\eg exp -> procExpr eg exp) eg exps
    procExpr eg _ = return eg -- ignore anything else

    -- main function that updates the graph with new edges
    updateGraphDep :: ExprGraph ->  C.Id -> GraphStateM ExprGraph
    updateGraphDep eg useVar =
        -- get the usage's def node from the useVar within the cur elem exprmap
        case (Map.lookup useVar (rExprMap topElem)) of
            -- add a dependency edge from the defUse to the current node
            Just useDefNode -> return $ addDep useDefNode expNode eg
            Nothing ->
                -- check to see if is the arg within an abs
                case (rTopExpr topElem) of
                    LTopAbs _ arg -> if (arg == useVar) then return eg else checkTopDep
                    -- if not, check the toplevel
                    _ -> checkTopDep
      where
        -- look in the top level for the expression instead
        checkTopDep = do
            topMap <- liftM rTopMap get
            case (Map.lookup useVar topMap) of
                Just useTopElem -> checkTopDep' useTopElem
                Nothing -> return eg -- TODO - should throwError if lookup fails
          where
            checkTopDep' useTopElem = do
                s <- get
                let tg' = addDep (rTopNode useTopElem) (rTopNode topElem) (rTopGraph s)
                put (s { rTopGraph = tg' }) -- put the updated tg back into StateM
                return eg -- return the unmodified eg

        addDep n1 n2 g = insEdge (n1, n2, ()) g



-- |Sorts the top and expressiosn graphs, returning an ordererd map representation of the model
-- also checks for recursive definitions at either level
sortGraphs :: TopGraph -> TopMap -> [(C.Id, C.Top C.Id)]
sortGraphs tg tm = trace (show res) []
  where
    res = sortExpr

    -- TODO - throw error if check fails
    sortTop = if sccCheck tg then topsort' tg else topsort' tg

    -- need to map over elems in sort top, extract the topmapElem, then sort the exprgraph and regen the top expr
    sortExpr = map sortExpr' sortTop

    -- TODO - throw error if check fails
    sortExpr' topVar = if exprCheck then (topVar, rE) else (topVar, rE)
      where
        topElem = (Map.!) tm topVar
        exprCheck = sccCheck (rExprGraph topElem)
        rE = reconExpr (rTopExpr topElem) (topsort' $ rExprGraph topElem) (rBaseExpr topElem)

    -- |function to check for stronglyConnComp within a graph, i.e. any recursive calls
    sccCheck = all (== 1) . map length . scc

    reconExpr :: TopExpr ->  [ExprNodeLabel] -> (C.Expr C.Id) -> C.Top C.Id
    reconExpr top lets base = case top of
        LTopLet i -> C.TopLet i (reconLets lets)
        LTopAbs i a ->  C.TopAbs i a (reconLets lets)
      where
        reconLets [] = base
        reconLets ((ExprNodeLabel i e):xs) = C.Let i e (reconLets xs)



























