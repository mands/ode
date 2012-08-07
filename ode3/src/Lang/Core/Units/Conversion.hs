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

-- splitUnit, simplifyUnits
) where

import Control.Monad.State (runState)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Graph.Inductive as G
import qualified Data.Graph.Inductive.NodeMap as NM
import qualified Data.Graph.Inductive.Query.BFS as BFS

import qualified Data.Foldable as DF

import Utils.CommonImports

import qualified Utils.Graph as UG
import Lang.Core.Units.UnitsDims

-- Conversion ----------------------------------------------------------------------------------------------------------
data ConvDef = ConvDef BaseUnit BaseUnit CExpr deriving (Show, Eq, Ord)

-- main graph type, nodes :: Units, edges :: CExprs
type ConvGraph = UG.GraphMap BaseUnit CExpr

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
    addConv cEnv convDef@(ConvDef fromUnit toUnit cExpr) = do
        -- get convData for the units
        dim <- getDimForBaseUnits fromUnit toUnit unitEnv
        -- insert edge and nodes into graph
        let convData' = updateGraph (getConvGraph dim cEnv) convDef
        -- update the convEnv
        return $ Map.insert dim convData' cEnv

    -- add the unit nodes and the conversion edge between them
    updateGraph :: ConvGraph -> ConvDef -> ConvGraph
    updateGraph cData convDef@(ConvDef fromUnit toUnit cExpr) = UG.runGraph_ cData $ do
        _ <- UG.insertNodeM_ fromUnit
        _ <- UG.insertNodeM_ toUnit
        NM.insMapEdgeM (fromUnit, toUnit, cExpr)


-- | Calculate, if possible, the conversion expression to use between two units
-- need to get the graph, calc the path between the nodes, then inline the expression
calcConvExpr :: Unit -> Unit -> UnitDimEnv -> ConvEnv -> MExcept CExpr
-- simply return "indentity function" in the case of equal units
calcConvExpr fromUnit toUnit uEnv cEnv | fromUnit == toUnit = return $ CFromId

-- TODO -- need to handle case of un-dimenstioned values explitictly here
calcConvExpr NoUnit toUnit uEnv cEnv = throwError $ printf "Cannot convert between units %s and %s" (show NoUnit) (show toUnit)
calcConvExpr fromUnit NoUnit uEnv cEnv = throwError $ printf "Cannot convert between units %s and %s" (show fromUnit) (show NoUnit)

calcConvExpr fromUnit@(UnitC _) toUnit@(UnitC _) uEnv cEnv = do
    -- check dimensions are valid
    dim <- getDimForUnits fromUnit toUnit uEnv
    let convGraph = getConvGraph dim cEnv
    undefined
--    -- get the path
--    n1 <- maybeToExcept (UG.getNodeInt convGraph fromUnit) $ printf "Unit %s not found in graph" (show fromUnit)
--    n2 <- maybeToExcept (UG.getNodeInt convGraph toUnit) $ printf "Unit %s not found in graph" (show toUnit)
--    let edges = UG.getEdgesFromPath $ BFS.lesp n1 n2 (UG.graph convGraph)
--
--    case length edges >= 1 of
--        True -> let expr = foldl1 inlineCExpr edges in
--            return $ trace' [MkSB expr]
--                (printf "Inlined Conversion expression %s => %s in Dim %s" (show fromUnit) (show toUnit) (show dim))
--                expr
--        False -> throwError $ printf "Cannot find conversion between units %s and %s" (show fromUnit) (show toUnit)

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

-- pos/neg split units map for each dimension
type SUMap = (Map.Map String Integer, Map.Map String Integer)

data SplitUnits = SplitUnits    { unitsDimL :: SUMap, unitsDimM :: SUMap, unitsDimT :: SUMap, unitsDimI :: SUMap
                                , unitsDimO :: SUMap, unitsDimJ :: SUMap, unitsDimN :: SUMap
                                } deriving (Show)

mkSplitUnits =  SplitUnits (Map.empty, Map.empty) (Map.empty, Map.empty) (Map.empty, Map.empty) (Map.empty, Map.empty)
                (Map.empty, Map.empty) (Map.empty, Map.empty) (Map.empty, Map.empty)

-- | Function to take a unit, and return both the pos and neg base units that make up the unit
splitUnit :: SrcUnit -> UnitDimEnv -> MExcept SplitUnits
splitUnit us uEnv = DF.foldlM splitUnit' mkSplitUnits us
  where
    splitUnit' sUnits (u, idx) = do
        -- get dim for unit
        dim <- lookupBUnitDim u uEnv
        -- update correct SUMap for dim
        sUnits' <- case dim of
            (DimVec 1 0 0 0 0 0 0) -> return $ sUnits { unitsDimL = updateSUMap (unitsDimL sUnits) u idx }
            (DimVec 0 1 0 0 0 0 0) -> return $ sUnits { unitsDimM = updateSUMap (unitsDimM sUnits) u idx }
            (DimVec 0 0 1 0 0 0 0) -> return $ sUnits { unitsDimT = updateSUMap (unitsDimT sUnits) u idx }
            (DimVec 0 0 0 1 0 0 0) -> return $ sUnits { unitsDimI = updateSUMap (unitsDimI sUnits) u idx }
            (DimVec 0 0 0 0 1 0 0) -> return $ sUnits { unitsDimO = updateSUMap (unitsDimO sUnits) u idx }
            (DimVec 0 0 0 0 0 1 0) -> return $ sUnits { unitsDimJ = updateSUMap (unitsDimJ sUnits) u idx }
            (DimVec 0 0 0 0 0 0 1) -> return $ sUnits { unitsDimN = updateSUMap (unitsDimN sUnits) u idx }
            _ -> throwError $ printf "Unexpected dimension %s found in base unit dimension map" (show dim)

        return sUnits'


-- | Takes an SUMap and a given base unit with index and returns the updated SUMap
updateSUMap (posUMap, negUMap) u idx = if idx >= 0
    then (insertUnit u idx posUMap, negUMap)
    else (posUMap, insertUnit u (negate idx) negUMap)
  where
    insertUnit = Map.insertWith (+)

-- need check units are correct dims
-- this is acutally as cast operation too
simplifyUnits fromUnit toUnit uEnv = do
    fromSplit <- splitUnit fromUnit uEnv
    toSplit <- splitUnit toUnit uEnv
    return $ unifySplits fromSplit toSplit
  where

    -- holds SplitData, sorted by dim, for both sides of equations, with pos idx only on both sides
    unifySplits fromSplit toSplit = SplitUnits  { unitsDimL = sortUnits (unitsDimL fromSplit) (unitsDimL toSplit)
                                                , unitsDimM = sortUnits (unitsDimM fromSplit) (unitsDimM toSplit)
                                                , unitsDimT = sortUnits (unitsDimT fromSplit) (unitsDimT toSplit)
                                                , unitsDimI = sortUnits (unitsDimI fromSplit) (unitsDimI toSplit)
                                                , unitsDimO = sortUnits (unitsDimO fromSplit) (unitsDimO toSplit)
                                                , unitsDimJ = sortUnits (unitsDimJ fromSplit) (unitsDimJ toSplit)
                                                , unitsDimN = sortUnits (unitsDimN fromSplit) (unitsDimN toSplit)
                                                }

    -- rearraange the equation so only pos on both sides
    sortUnits (fromPos, fromNeg) (toPos, toNeg) = (Map.unionWith (+) fromPos toNeg, Map.unionWith (+) toPos fromNeg)































