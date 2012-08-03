-----------------------------------------------------------------------------
--
-- Module      :  Lang.Core.Units.Conversion
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Code to handle units conversion, graph creation, casting, simplfication, etc.
--
-----------------------------------------------------------------------------

module Lang.Core.Units.Conversion (
-- datatypes
ConvDef(..), ConvGraph(..), ConvEnv(..), CExpr(..), COp(..),

-- main functions
addConvsToGraph, calcConvExpr,
) where

import Control.Monad.State (runState)

import qualified Data.Map as Map
import qualified Data.Foldable as DF
import qualified Data.Graph.Inductive as G
import qualified Data.Graph.Inductive.NodeMap as NM
import qualified Data.Graph.Inductive.Query.BFS as BFS

import Utils.CommonImports

import qualified Utils.Graph as UG
import Lang.Core.Units.UnitsDims

-- Conversion ----------------------------------------------------------------------------------------------------------
data ConvDef = ConvDef Unit Unit CExpr deriving (Show, Eq, Ord)

-- main graph type, nodes :: Units, edges :: CExprs
type ConvGraph = UG.GraphMap Unit CExpr

type ConvEnv = Map.Map DimVec ConvGraph

-- create a new conversion graph for the dim if doesn't exist
getConvGraph :: DimVec -> ConvEnv -> ConvGraph
getConvGraph dim cEnv = case (Map.lookup dim cEnv) of
                            Nothing -> UG.mkGraphMap
                            Just x -> x

getConvGraphForUnits :: Unit -> Unit -> UnitDimEnv -> ConvEnv -> MExcept ConvGraph
getConvGraphForUnits u1 u2 uEnv cEnv = getConvGraph <$> (getDimForUnits u1 u2 uEnv) <*> pure cEnv

--type ConversionFactor =
addConvsToGraph :: ConvEnv -> [ConvDef] -> UnitDimEnv -> MExcept ConvEnv
addConvsToGraph cEnv convs unitEnv = DF.foldlM addConv cEnv convs
  where
    addConv :: ConvEnv -> ConvDef -> MExcept ConvEnv
    addConv cEnv convDef@(ConvDef fromUnit toUnit cExpr) | isBaseUnit fromUnit && isBaseUnit toUnit = do
        -- get convData for the units
        dim <- getDimForUnits fromUnit toUnit unitEnv
        -- insert edge and nodes into graph
        let convData' = updateGraph (getConvGraph dim cEnv) convDef
        -- update the convEnv
        return $ Map.insert dim convData' cEnv

    addConv cEnv convDef@(ConvDef fromUnit toUnit cExpr) | otherwise =
        errorDump [MkSB fromUnit, MkSB toUnit] $ printf "Found convDefs using dervied units"

    -- add the unit nodes and the conversion edge between them
    updateGraph :: ConvGraph -> ConvDef -> ConvGraph
    updateGraph cData convDef@(ConvDef fromUnit toUnit cExpr) = UG.runGraph_ cData $ do
        _ <- UG.insertNodeM_ fromUnit
        _ <- UG.insertNodeM_ toUnit
        NM.insMapEdgeM (fromUnit, toUnit, cExpr)


-- | Calculate, if possible, the conversion expression to use between two units
-- need to get the graph, calc the path between the nodes, then inline the expression
calcConvExpr :: Unit -> Unit -> UnitDimEnv -> ConvEnv -> MExcept CExpr
-- TODO -- need to handle case of un-dimenstioned values explitictly here
-- we simply return the identty of the source variable in the case of NoUnits
calcConvExpr NoUnit NoUnit uEnv cEnv = return $ CFromId
calcConvExpr NoUnit toUnit uEnv cEnv = throwError $ printf "Cannot convert between units %s and %s" (show NoUnit) (show toUnit)
calcConvExpr fromUnit NoUnit uEnv cEnv = throwError $ printf "Cannot convert between units %s and %s" (show fromUnit) (show NoUnit)

calcConvExpr fromUnit@(UnitC _) toUnit@(UnitC _) uEnv cEnv = do
    -- get the graph
    dim <- getDimForUnits fromUnit toUnit uEnv
    let convGraph = getConvGraph dim cEnv

    -- get the path
    n1 <- maybeToExcept (UG.getNodeInt convGraph fromUnit) $ printf "Unit %s not found in graph" (show fromUnit)
    n2 <- maybeToExcept (UG.getNodeInt convGraph toUnit) $ printf "Unit %s not found in graph" (show toUnit)
    let edges = UG.getEdgesFromPath $ BFS.lesp n1 n2 (UG.graph convGraph)

    case length edges >= 1 of
        True -> let expr = foldl1 inlineCExpr edges in
            return $ trace' [MkSB expr]
                (printf "Inlined Conversion expression %s => %s in Dim %s" (show fromUnit) (show toUnit) (show dim))
                expr
        False -> throwError $ printf "Cannot find conversion between units %s and %s" (show fromUnit) (show toUnit)

-- | restricted expression AST for conversion functions
-- where ConvFromId is the static identifier for the source unit value
data CExpr = CExpr COp CExpr CExpr | CNum Double | CFromId deriving (Show, Eq, Ord)
data COp = CAdd | CSub | CMul | CDiv deriving (Show, Eq, Ord)

-- | Inline one expression into the next, using the CFromId as the insertion point within destExpr
inlineCExpr :: CExpr -> CExpr -> CExpr
inlineCExpr srcExpr CFromId = srcExpr
inlineCExpr srcExpr (CExpr op e1 e2) = CExpr op (inlineCExpr srcExpr e1) (inlineCExpr srcExpr e2)
inlineCExpr _ destExpr = destExpr



-- New Simplification and Conversion -----------------------------------------------------------------------------------


--data






















































