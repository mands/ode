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
--
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
import qualified Core.AST as C
import Utils.Utils

-- define the types we need for our graphs
-- how do they relate? they don't - are indepdent - makes things easier
-- we need some maps to hold the non-let body of an expr
-- need to reconstruct the expressions again after sorting

-- need a topgraph and a topmap - place them both in a state monad and done
type TopGraph = Gr TopNodeLabel ()
data TopNodeLabel = LTopLet C.Id | LTopAbs C.Id C.Id deriving Show

type ExprGraph = Gr ExprNodeLabel ()
data ExprNodeLabel = ExprNodeLabel C.Id (C.Expr C.Id) deriving Show

-- for a top-level id, -> node int, baseexpr, (exprmap - varid -> (node int)), exprgraph
type TopMap = Map.Map C.Id TopMapElem
data TopMapElem = TopMapElem {  rTopNode :: Int, rAbsArg :: Maybe C.Id, rBaseExpr :: (C.Expr C.Id),
                                rExprMap :: Map.Map C.Id Int, rExprGraph :: ExprGraph }
                                deriving Show

-- state monad
data ReorderState = ReorderState { rTopGraph :: TopGraph, rTopMap :: TopMap}
type GraphStateM = State ReorderState

reorder :: C.Model C.Id -> MExcept (C.Model C.Id)
reorder cModel = (trace (show topGraph ++ "\n") (trace (Map.showTree topMap) (trace (show topGraph' ++ "\n") (trace (Map.showTree topMap') Right cModel))))
  where
    (topGraph', topMap') = procDepGraphs topGraph topMap
    topGraph = createTopGraph cModel topMap
    topMap = createTopMap cModel


-- |The main top-level graph
createTopGraph :: C.Model C.Id -> TopMap -> TopGraph
createTopGraph cModel topMap = mkGraph topGraphNodes []
  where
    -- get list of top-graph nodes - need to make sure this matches up with TopMap
    topGraphNodes = convGTop <$> (Map.elems cModel) -- use applicative style
    convGTop (C.TopLet i exp) = (rTopNode $ (Map.!) topMap i, LTopLet i)
    convGTop (C.TopAbs i a exp) = (rTopNode $ (Map.!) topMap i, LTopAbs i a)


-- |need to build the TopMap, can use the model to do this
-- fold over the elements, creating a new topmap thru accumulation
-- TODO - cleanup
createTopMap :: C.Model C.Id -> TopMap
createTopMap cModel = topMap
  where
    (_, topMap) = Map.mapAccum createTopMapElem [1..] cModel

    createTopMapElem (x:xs) (C.TopLet i exp) = (xs, TopMapElem { rTopNode = x, rAbsArg = Nothing, rBaseExpr = baseExpr, rExprMap = map, rExprGraph = createExprGraph exp})
      where
        (baseExpr, map) = (createExprMap [1..] exp)

    createTopMapElem (x:xs) (C.TopAbs i a exp) = (xs, TopMapElem { rTopNode = x, rAbsArg = Just a, rBaseExpr = baseExpr, rExprMap = map, rExprGraph = createExprGraph exp})
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

-- |Process an individual expression by pattern match over the possibilities
-- note - we have already removed all lets from the expressiong so no defs possible
  where
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
                case (rAbsArg topElem) of
                    Just arg -> if (arg == useVar) then return eg else checkTopDep
                    -- if not, check the toplevel
                    Nothing -> checkTopDep
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
