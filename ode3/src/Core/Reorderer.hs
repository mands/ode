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

module Core.Reorderer (
reorder
) where

import Control.Monad.State
import Control.Monad.Error

import qualified Data.Map as Map
import qualified Data.Traversable as DT
import Debug.Trace
import Control.Applicative
import Data.Graph.Inductive
import Data.Graph.Inductive.Tree -- TODO - maybe switch to PatriciaTree implementation?
import Data.Graph.Inductive.Query.DFS
import Data.Maybe (fromJust)

import qualified Core.ExprAST as E
import qualified Core.ModuleAST as M
import Utils.Utils
import qualified Utils.OrdMap as OrdMap

-- define the types we need for our graphs
-- need a topgraph and a topmap - place them both in a state monad and done
type TopGraph = Gr (E.Bind E.SrcId) ()
data TopExpr = LTopLet (E.Bind E.SrcId) | LTopAbs (E.Bind E.SrcId) E.SrcId  deriving Show

type ExprGraph = Gr ExprNodeLabel ()
data ExprNodeLabel = ExprNodeLabel (E.Bind E.SrcId) (E.Expr E.SrcId) deriving Show

-- for a top-level id, -> node int, baseexpr, (exprmap - varid -> (node int)), exprgraph
-- we need some maps to hold the non-let body of an expr
type TopMap = Map.Map (E.Bind E.SrcId) TopMapElem
-- a two-stage binding map to map individual binds - need to abstract this datastructure out
type TopBindMap = Map.Map E.SrcId (E.Bind E.SrcId)

data TopMapElem = TopMapElem {  rTopNode :: Int, rTopExpr :: TopExpr, rBaseExpr :: (E.Expr E.SrcId),
                                rExprMap :: Map.Map E.SrcId Int, rExprGraph :: ExprGraph }
                                deriving Show

-- state monad
data ReorderState = ReorderState { rTopGraph :: TopGraph, rTopMap :: TopMap, rTopBindMap :: TopBindMap}
type GraphStateM = StateT ReorderState MExcept
newtype GraphStateMa a = GraphStateMa { runReorder :: StateT ReorderState (MExcept) a }
    deriving (Monad, MonadState ReorderState, MonadError String)

reorder :: M.Module E.SrcId -> MExcept (M.Module E.SrcId)
reorder (M.LitMod exprMap modData) = reorder' exprMap >>= (\exprMap -> (return $ M.LitMod exprMap modData))
reorder (M.FunctorMod args exprMap modData) = reorder' exprMap >>= (\exprMap -> (return $ M.FunctorMod args exprMap modData))

reorder' :: M.ExprMap E.SrcId -> MExcept (M.ExprMap E.SrcId)
reorder' exprMap = do
    -- build the dependency graphs
    (topGraph', topMap') <- procDepGraphs topGraph topMap topBindMap
    -- now we need to sort the graphs and reconstruct the expressions
    exprMap' <- sortGraphs topGraph' topMap'
    return $ trace ("(RO) " ++ (show exprMap')) exprMap'
  where
    topGraph = createTopGraph exprMap topMap
    topMap = createTopMap exprMap
    topBindMap = createTopBindMap topMap

-- | The main top-level graph
createTopGraph :: M.ExprMap E.SrcId -> TopMap -> TopGraph
createTopGraph exprMap topMap = mkGraph topGraphNodes []
  where
    -- get list of top-graph nodes - need to make sure this matches up with TopMap
    topGraphNodes = convGTop <$> (OrdMap.elems exprMap) -- use applicative style
    -- create a list of the node id (int), and the binding val as the node info
    convGTop (E.TopLet i exp) = (rTopNode $ topMap Map.! i, i)
    convGTop (E.TopAbs i a exp) = (rTopNode $ topMap Map.! i, i)

-- TODO should check here for duplicated ids and throw errors
-- | need to build the TopMap, can use the model to do this
-- fold over the elements, creating a new topmap thru accumulation
createTopMap :: M.ExprMap E.SrcId -> TopMap
createTopMap exprMap = topMap
  where
    (_, topMap) = Map.mapAccum createTopMapElem [1..] (OrdMap.toMap exprMap)

    createTopMapElem (x:xs) (E.TopLet i exp) = (xs, TopMapElem { rTopNode = x, rTopExpr = LTopLet i, rBaseExpr = baseExpr, rExprMap = map, rExprGraph = createExprGraph exp})
      where
        (baseExpr, map) = (createExprMap [1..] exp)

    createTopMapElem (x:xs) (E.TopAbs i a exp) = (xs, TopMapElem { rTopNode = x, rTopExpr = LTopAbs i a, rBaseExpr = baseExpr, rExprMap = map, rExprGraph = createExprGraph exp})
      where
        (baseExpr, map) = (createExprMap [1..] exp)

    createExprMap (x:xs) (E.Let (E.LetBind bs) e1 e2) = (baseExpr, foldl (\map b -> Map.insert b x map) map' bs)
      where
        (baseExpr, map') = (createExprMap xs e2)

    createExprMap (x:xs) baseExpr = (baseExpr, Map.empty)

    -- function to create expression graph
    createExprGraph e = mkGraph (zip [1..] (convGExpr e)) []
    convGExpr (E.Let i e1 e2) = (ExprNodeLabel i e1) : (convGExpr e2)
    convGExpr _ = []


createTopBindMap :: TopMap -> TopBindMap
createTopBindMap topMap = foldl createElem Map.empty (Map.keys topMap)
  where
    createElem map bind@(E.AbsBind b) = Map.insert b bind map
    createElem map bind@(E.LetBind bs) = foldl (\map b -> Map.insert b bind map) map bs


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
    procExprNode :: (E.Bind E.SrcId, TopMapElem) -> LNode ExprNodeLabel -> GraphStateMa (E.Bind E.SrcId, TopMapElem)
    procExprNode (topVar, topElem) (expNode, ExprNodeLabel expVar exp) =
        procExprN topElem (rExprGraph topElem) (Just expNode) exp >>= (\eg -> return (topVar, topElem {rExprGraph = eg}))

    -- | Creates dependencies for the base expression within an expression
    procBaseExpr :: (E.Bind E.SrcId, TopMapElem) -> GraphStateMa (E.Bind E.SrcId, TopMapElem)
    procBaseExpr (topVar, topElem) =
        procExprN topElem (rExprGraph topElem) Nothing (rBaseExpr topElem) >>= (\eg -> return (topVar, topElem {rExprGraph = eg}))


-- | Process an individual expression node within the given expression graph eg
procExprN :: TopMapElem -> ExprGraph -> Maybe Node -> (E.Expr E.SrcId) -> GraphStateMa ExprGraph
procExprN topElem eg mENode exp = procExpr eg exp
  where
    -- NOTE - we have already removed all lets from the expressiong so no defs possible
    -- | Process an individual expression by pattern match over the possibilities
    procExpr :: ExprGraph -> E.Expr E.SrcId -> GraphStateMa ExprGraph
    procExpr eg (E.Var (E.LocalVar useVar)) = updateGraphDep eg useVar -- create a link in the graph from def to use
    procExpr eg (E.App (E.LocalVar useVar) exp) = (updateGraphDep eg useVar) >>= (\eg -> procExpr eg exp)  -- create a link in the graph from def to use
    procExpr eg (E.Op _ exp) = procExpr eg exp
    procExpr eg (E.If exp1 exp2 exp3) = foldM (\eg exp -> procExpr eg exp) eg [exp1,exp2,exp3]
    procExpr eg (E.Tuple exps) = foldM (\eg exp -> procExpr eg exp) eg exps
    procExpr eg _ = return eg -- ignore anything else, i.e. module references, as they are already defined

    -- | main function that updates the graph with new edges
    -- check all contexts, get the usage's def node from the useVar within the cur elem exprmap
    updateGraphDep :: ExprGraph -> E.SrcId -> GraphStateMa ExprGraph
    updateGraphDep eg useVar = case (Map.lookup useVar (rExprMap topElem)) of
        -- are we refercing ourselves?
        -- check for binding to outside scope with same name as local scope, if so don't create recursive loop
        Just useDefNode | (Just useDefNode == mENode) -> do
            mUseTopElem <- topBindLookup useVar
            maybe (addLocalDep useDefNode) (\useTopElem -> addTopDep useTopElem) mUseTopElem

        -- add a dependency edge from the defUse to the current node
        -- if current expNode exists, i.e. is not a baseExpr
        Just useDefNode | otherwise -> addLocalDep useDefNode

        -- ref not within current scope, check special cases, if not check toplevel
        Nothing -> case (rTopExpr topElem) of
            -- is the ref to the arg within an abs
            LTopAbs _ arg | (arg == useVar) -> return eg
            -- if not, look in the top level for the expression instead
            _ -> do
                mUseTopElem <- topBindLookup useVar
                maybe (throwError $ "(RO03) Referenced variable " ++ (show useVar) ++ " not found")
                    (\useTopElem -> addTopDep useTopElem) mUseTopElem
      where
        -- | add a dependency from n1 to n2 within the graph g
        addDep n1 n2 g = insEdge (n1, n2, ()) g
        addLocalDep useDefNode = return $ maybe eg (\eNode -> addDep useDefNode eNode eg) mENode
        addTopDep useTopElem = do
            s <- get
            let tg' = addDep (rTopNode useTopElem) (rTopNode topElem) (rTopGraph s)
            put (s { rTopGraph = tg' }) -- put the updated tg back into StateM
             -- return the unmodified eg
            return eg

        -- helper function, checks in both binding map levels, threading the maybe thru
        topBindLookup :: E.SrcId -> GraphStateMa (Maybe TopMapElem)
        topBindLookup v = do
            s <- get
            return $ (Map.!) <$> (pure $ rTopMap s) <*> (Map.lookup v (rTopBindMap s))

-- | Sorts the top and expressiosn graphs, returning an ordererd map representation of the model
-- also checks for recursive definitions at either level
sortGraphs :: TopGraph -> TopMap -> MExcept (M.ExprMap E.SrcId)
sortGraphs tg tm = do
    sortedTops <- if simpleCheck tg then return (topsort' tg)
        -- TODO - need to determine the names of the elements
        else throwError "(RO01) Found recursive relationship between top-level elements"

    -- need to map over elems in sort top, extract the topmapElem, then sort the exprgraph and regen the top expr
    liftM OrdMap.fromList $ DT.mapM sortExpr sortedTops
  where
    sortExpr topVar = if exprCheck then return (topVar, rE)
        else throwError ("(RO02) Found recursive relationship between expressions within " ++ (show topVar))
      where
        topElem = maybe (error "(ROO4)") id $ Map.lookup topVar tm
        exprCheck = simpleCheck (rExprGraph topElem)
        rE = reconExpr (rTopExpr topElem) (topsort' $ rExprGraph topElem) (rBaseExpr topElem)

    -- | function to check for loops and/or stronglyConnComp within a graph, i.e. any recursive calls
    simpleCheck g = isSimple g && (all (== 1) . map length . scc $ g)

    -- | Reconstructs an expression from the list of lets and the baseExpr
    reconExpr :: TopExpr ->  [ExprNodeLabel] -> (E.Expr E.SrcId) -> E.Top E.SrcId
    reconExpr top lets base = case top of
        LTopLet i -> E.TopLet i (reconLets lets)
        LTopAbs i a ->  E.TopAbs i a (reconLets lets)
      where
        reconLets [] = base
        reconLets ((ExprNodeLabel i e):xs) = E.Let i e (reconLets xs)

