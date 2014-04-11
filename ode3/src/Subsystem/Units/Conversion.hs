-----------------------------------------------------------------------------
--
-- Module      :  Subsystem.Units.Conversion
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  BSD3
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Code to handle units conversion, graph creation, casting, simplfication, etc.
--
-----------------------------------------------------------------------------

module Subsystem.Units.Conversion (
-- datatypes
ConvDef(..), ConvGraph, ConvEnv, CExpr(..), COp(..),

-- main functions
addConvsToGraph, convertCastUnit,

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
import Subsystem.Units.UnitsDims

-- Conversion Expressions ----------------------------------------------------------------------------------------------

-- | restricted expression AST for conversion functions
-- where ConvFromId is the static identifier for the source unit value
data CExpr = CExpr COp CExpr CExpr | CNum Double | CFromId deriving (Show, Eq, Ord)
data COp = CAdd | CSub | CMul | CDiv deriving (Show, Eq, Ord)

-- | Inline one expression into the next, using the CFromId as the insertion point within destExpr
inlineCExpr :: CExpr -> CExpr -> CExpr
inlineCExpr srcExpr CFromId = srcExpr
inlineCExpr srcExpr (CExpr op e1 e2) = CExpr op (inlineCExpr srcExpr e1) (inlineCExpr srcExpr e2)
inlineCExpr _ destExpr = destExpr

-- Conversion Datatypes ------------------------------------------------------------------------------------------------
data ConvDef = ConvDef BaseUnit BaseUnit CExpr deriving (Show, Eq, Ord)

-- main graph type, nodes :: Units, edges :: CExprs
type ConvGraph = UG.GraphMap BaseUnit CExpr

type ConvEnv = Map.Map BaseDim ConvGraph

-- Conversion Graphs ---------------------------------------------------------------------------------------------------


-- create a new conversion graph for the dim if doesn't exist
getConvGraph :: BaseDim -> ConvEnv -> ConvGraph
getConvGraph dim cEnv = case (Map.lookup dim cEnv) of
                            Nothing -> UG.mkGraphMap
                            Just x -> x

getConvGraphForUnits :: BaseUnit -> BaseUnit -> UnitDimEnv -> ConvEnv -> MExcept ConvGraph
getConvGraphForUnits u1 u2 uEnv cEnv = getConvGraph <$> (getBDimForBUnits u1 u2 uEnv) <*> pure cEnv

--type ConversionFactor =
addConvsToGraph :: ConvEnv -> [ConvDef] -> UnitDimEnv -> MExcept ConvEnv
addConvsToGraph cEnv convs unitEnv = DF.foldlM addConv cEnv convs
  where
    addConv :: ConvEnv -> ConvDef -> MExcept ConvEnv
    addConv cEnv convDef@(ConvDef fromUnit toUnit cExpr) = do
        -- get convData for the units
        dim <- getBDimForBUnits fromUnit toUnit unitEnv
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

-- actually find/derive a conversion expression between two base units from the conversion graph
convertBaseUnit :: BaseUnit -> BaseUnit -> ConvGraph -> MExcept CExpr
convertBaseUnit u1 u2 convGraph = do
    -- find the conversion path within the graph
    n1 <- maybeToExcept (UG.getNodeInt convGraph u1) $ printf "Unit %s not found in graph" u1
    n2 <- maybeToExcept (UG.getNodeInt convGraph u2) $ printf "Unit %s not found in graph" u2
    let edges = UG.getEdgesFromPath $ BFS.lesp n1 n2 (UG.graph convGraph)

    -- create the conversion expressions
    case length edges >= 1 of
        True -> let expr = foldl1 inlineCExpr edges in
            return $ expr -- trace' [MkSB expr] (printf "Inlined Conversion expression %s => %s" u1 u2) expr
        False -> throwError $ printf "Cannot find conversion between units %s and %s" u1 u2


-- Conversion between Units (inc Simplification and derived units conversion) ------------------------------------------

type BUMap = Map.Map BaseUnit Integer
type SUMap = (BUMap, BUMap)
-- pos/neg split units map for each dimension
data SplitUnit = SplitUnit  { unitsDimL :: SUMap, unitsDimM :: SUMap, unitsDimT :: SUMap, unitsDimI :: SUMap
                            , unitsDimO :: SUMap, unitsDimJ :: SUMap, unitsDimN :: SUMap
                            } deriving (Show)

mkSplitUnits =  SplitUnit (Map.empty, Map.empty) (Map.empty, Map.empty) (Map.empty, Map.empty) (Map.empty, Map.empty)
                (Map.empty, Map.empty) (Map.empty, Map.empty) (Map.empty, Map.empty)

-- | Function to take a unit, and return both the pos and neg base units that make up the unit
splitUnit :: UnitList -> UnitDimEnv -> MExcept SplitUnit
splitUnit us uEnv = DF.foldlM splitUnit' mkSplitUnits us
  where
    splitUnit' sUnits (u, idx) = do
        -- get dim for unit
        dim <- lookupBUnitDim u uEnv
        -- update correct SUMap for dim
        sUnits' <- case dim of
            DimL -> return $ sUnits { unitsDimL = updateSUMap (unitsDimL sUnits) u idx }
            DimM -> return $ sUnits { unitsDimM = updateSUMap (unitsDimM sUnits) u idx }
            DimT -> return $ sUnits { unitsDimT = updateSUMap (unitsDimT sUnits) u idx }
            DimI -> return $ sUnits { unitsDimI = updateSUMap (unitsDimI sUnits) u idx }
            DimO -> return $ sUnits { unitsDimO = updateSUMap (unitsDimO sUnits) u idx }
            DimJ -> return $ sUnits { unitsDimJ = updateSUMap (unitsDimJ sUnits) u idx }
            DimN -> return $ sUnits { unitsDimN = updateSUMap (unitsDimN sUnits) u idx }
        return sUnits'

    -- | Takes an SUMap and a given base unit with index and returns the updated SUMap
    updateSUMap (posUMap, negUMap) u idx = if idx >= 0
        then (insertUnit u idx posUMap, negUMap)
        else (posUMap, insertUnit u (negate idx) negUMap)
      where
        insertUnit = Map.insertWith (+)


-- | Calculate, if possible, the conversion expression to use between two units
-- need to get the graph, calc the path between the nodes, then inline the expression
-- this is acutally as cast and simplification operation too
convertCastUnit :: Unit -> Unit -> UnitDimEnv -> ConvEnv -> MExcept CExpr
-- simply return "identity function" in the case of equal units
convertCastUnit fromUnitC@(UnitC fromUnit) toUnitC@(UnitC toUnit) uEnv cEnv | fromUnit == toUnit = return $ CFromId

convertCastUnit fromUnitC@(UnitC fromUnit) toUnitC@(UnitC toUnit) uEnv cEnv = do
    -- check dimensions are valid (TODO - may not be needed as only called after unit-check)
    _ <- getDimForUnits fromUnitC toUnitC uEnv

    -- get the split the units across base dimensions
    fromSplit <- splitUnit fromUnit uEnv
    toSplit <- splitUnit toUnit uEnv

    -- now for each dim, calculate the conv expr, and inline them all and return
    convertUnitsInDim (unitsDimL fromSplit) (unitsDimL toSplit) DimL CFromId
        >>= convertUnitsInDim (unitsDimM fromSplit) (unitsDimM toSplit) DimM
        >>= convertUnitsInDim (unitsDimT fromSplit) (unitsDimT toSplit) DimT
        >>= convertUnitsInDim (unitsDimI fromSplit) (unitsDimI toSplit) DimI
        >>= convertUnitsInDim (unitsDimO fromSplit) (unitsDimO toSplit) DimO
        >>= convertUnitsInDim (unitsDimJ fromSplit) (unitsDimJ toSplit) DimJ
        >>= convertUnitsInDim (unitsDimN fromSplit) (unitsDimN toSplit) DimN

  where
    -- main conveersion expression for a set BaseDim
    -- simple impleemtatino for now - other implementations are possible
    -- for each posU, if same dim exists on other side, try cancel, else pick a negU at random and try convert
    convertUnitsInDim :: SUMap -> SUMap -> BaseDim -> CExpr -> MExcept CExpr
    convertUnitsInDim lhsSUMap rhsSUMap dim cExpr = do
        -- first rearrange
        let (lhsUs, rhsUs) = rearrangeUnits lhsSUMap rhsSUMap
        -- trace' [MkSB lhsUs, MkSB rhsUs] "rearranged units" $ return ()

        -- then cancel
        let (lhsUs', rhsUs') = cancelUnits lhsUs rhsUs
        -- trace' [MkSB lhsUs', MkSB rhsUs'] "after cancelling units" $ return ()

        -- now convert the remaining, one-by-one from the lhs
        (rhsUs'', cExpr') <- DF.foldlM convertSingleUnit (rhsUs', cExpr) $ Map.toAscList lhsUs'
        -- check we converted all vals
        unless (Map.null rhsUs'') $ errorDump [MkSB lhsUs, MkSB rhsUs, MkSB rhsUs''] "(UC) Final rhs map not empty after sucessful conversion" assert

        return cExpr'
      where

        -- rearraange the equation so only pos on both sides
        rearrangeUnits (fromPos, fromNeg) (toPos, toNeg) = (Map.unionWith (+) fromPos toNeg, Map.unionWith (+) toPos fromNeg)

        -- cancel common units on both sides
        cancelUnits lhsUs rhsUs = (lhsUs', rhsUs')
          where
            commonUnits = Map.intersectionWith min lhsUs rhsUs
            lhsUs' = Map.differenceWith fDiff lhsUs commonUnits
            rhsUs' = Map.differenceWith fDiff rhsUs commonUnits
            fDiff a b = let diff = a - b in if (diff == 0) then Nothing else Just diff

        convertSingleUnit :: (BUMap, CExpr) -> (BaseUnit, Integer) -> MExcept (BUMap, CExpr)
        convertSingleUnit s@(rhsUs, cExpr) (lhsUnit, lIdx) =
            -- trace' [MkSB lhsUnit, MkSB lIdx, MkSB rhsUs] "convU loop" $
            if lIdx > 0
                then convertSingleUnit' >>= (\s -> convertSingleUnit s (lhsUnit, lIdx - 1)) -- execute the conversion monad and call again
                else return s
          where
            convertSingleUnit' = do
                -- get a base unit from rhs
                let ((rhsUnit, rIdx), rhsUs') = case Map.minViewWithKey rhsUs of
                                                    Just x  -> x -- trace' [MkSB x] "rhs selected unit" $ x
                                                    Nothing -> errorDump [MkSB rhsUs] "(UC) Cannot find anymore rhs units" assert

                cExpr' <- convertBaseUnit lhsUnit rhsUnit (getConvGraph dim cEnv)
                -- trace' [MkSB lhsUnit, MkSB rhsUnit, MkSB cExpr'] "derived conversion expr" $ return ()
                -- return the updated rhs map minus an index, and the inlined conversion expr
                let rhsUs'' = if (rIdx == 1) then rhsUs' else Map.insert rhsUnit (rIdx-1) rhsUs'
                return (rhsUs'', inlineCExpr cExpr cExpr')

convertCastUnit fromUnit toUnit uEnv cEnv = throwError $ printf "Cannot convert between units %s and %s" (show fromUnit) (show toUnit)

