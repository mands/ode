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

type ConvEnv = Map.Map BaseDim ConvGraph

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
    -- let convGraph = getConvGraph dim cEnv
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
type BUMap = Map.Map BaseUnit Integer
type SUMap = (BUMap, BUMap)
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

-- need check units are correct dims
-- this is acutally as cast operation too

simplifyUnits :: Unit -> Unit -> UnitDimEnv -> ConvEnv -> MExcept CExpr
simplifyUnits fromUnitC@(UnitC fromUnit) toUnitC@(UnitC toUnit) uEnv cEnv = do
    -- check dimensions are valid
    _ <- getDimForUnits fromUnitC toUnitC uEnv
    -- let convGraph = getConvGraph dim cEnv

    -- get the unified split
    uSplit <- unifySplits <$> (splitUnit fromUnit uEnv) <*> (splitUnit toUnit uEnv)
    _ <- trace' [MkSB uSplit] "rearranged split units" (return ())
    -- now for each dim, calculate the conv expr, and inline them all
    cExpr <-    calcConvExpr' (unitsDimL uSplit) (getConvGraph DimL cEnv) CFromId
                >>= calcConvExpr' (unitsDimM uSplit) (getConvGraph DimM cEnv)
                >>= calcConvExpr' (unitsDimT uSplit) (getConvGraph DimT cEnv)
                >>= calcConvExpr' (unitsDimI uSplit) (getConvGraph DimI cEnv)
                >>= calcConvExpr' (unitsDimO uSplit) (getConvGraph DimO cEnv)
                >>= calcConvExpr' (unitsDimJ uSplit) (getConvGraph DimJ cEnv)
                >>= calcConvExpr' (unitsDimN uSplit) (getConvGraph DimN cEnv)

    return cExpr
  where

    -- holds SplitData, sorted by dim, for both sides of equations, with pos idx only on both sides
    unifySplits fromSplit toSplit = SplitUnits  { unitsDimL = rearrangeUnits (unitsDimL fromSplit) (unitsDimL toSplit)
                                                , unitsDimM = rearrangeUnits (unitsDimM fromSplit) (unitsDimM toSplit)
                                                , unitsDimT = rearrangeUnits (unitsDimT fromSplit) (unitsDimT toSplit)
                                                , unitsDimI = rearrangeUnits (unitsDimI fromSplit) (unitsDimI toSplit)
                                                , unitsDimO = rearrangeUnits (unitsDimO fromSplit) (unitsDimO toSplit)
                                                , unitsDimJ = rearrangeUnits (unitsDimJ fromSplit) (unitsDimJ toSplit)
                                                , unitsDimN = rearrangeUnits (unitsDimN fromSplit) (unitsDimN toSplit)
                                                }

    -- rearraange the equation so only pos on both sides
    rearrangeUnits (fromPos, fromNeg) (toPos, toNeg) = (Map.unionWith (+) fromPos toNeg, Map.unionWith (+) toPos fromNeg)

    -- main conveersion expression
    -- simple impleemtatino for now - other implementations are possible
    -- for each posU, if same dim exists on other side, try cancel, else pick a negU at random and try convert
    calcConvExpr' :: SUMap -> ConvGraph -> CExpr -> MExcept CExpr
    calcConvExpr' (lhsUs, rhsUs) convGraph cExpr = do

        -- first cancel
        let commonUnits = Map.intersectionWith min lhsUs rhsUs
        let lhsUs' = Map.differenceWith (\a b -> let diff = a - b in if (diff == 0) then Nothing else Just diff) lhsUs commonUnits
        let rhsUs' = Map.differenceWith (\a b -> let diff = a - b in if (diff == 0) then Nothing else Just diff) rhsUs commonUnits

        _ <- trace' [MkSB commonUnits, MkSB lhsUs', MkSB rhsUs'] "after cancelling units" (return ())

        -- now convert remaining
        (rhsUs'', cExpr') <- DF.foldlM convU (rhsUs', cExpr) $ Map.toAscList lhsUs'
        -- check we converted all vals
        _ <- if (Map.null rhsUs'') then return () else errorDump [MkSB lhsUs, MkSB rhsUs, MkSB rhsUs''] "(UC) Final rhs map not empty after sucessful conversion"

        return cExpr'
      where
        convU :: (BUMap, CExpr) -> (BaseUnit, Integer) -> MExcept (BUMap, CExpr)
        convU s@(rhsUs, cExpr) (lhsUnit, lIdx) =
            trace' [MkSB lhsUnit, MkSB lIdx, MkSB rhsUs] "convU loop" $ if lIdx > 0
                then convU' >>= (\s -> convU s (lhsUnit, lIdx - 1)) -- execute the conversion monad and call again
                else return s
          where
            convU' = do
                -- get a base unit from rhs
                let ((rhsUnit, rIdx), rhsUs') = case Map.minViewWithKey rhsUs of
                                                    Just x  -> trace' [MkSB x] "rhs selected unit" $ x
                                                    Nothing -> errorDump [MkSB rhsUs] "(UC) Cannot find anymore rhs units"

                cExpr' <- convertUnits lhsUnit rhsUnit
                _ <- trace' [MkSB lhsUnit, MkSB rhsUnit, MkSB cExpr'] "dervied conversion expr" $ return ()
                -- return the updated rhs map minus an index, and the inlined conversion expr
                let rhsUs'' = if (rIdx == 1) then rhsUs' else Map.insert rhsUnit (rIdx-1) rhsUs'
                return (rhsUs'', inlineCExpr cExpr cExpr')

        convertUnits :: BaseUnit -> BaseUnit -> MExcept CExpr
        convertUnits u1 u2 = do
            -- find the conversion path within the graph
            n1 <- maybeToExcept (UG.getNodeInt convGraph u1) $ printf "Unit %s not found in graph" u1
            n2 <- maybeToExcept (UG.getNodeInt convGraph u2) $ printf "Unit %s not found in graph" u2
            let edges = UG.getEdgesFromPath $ BFS.lesp n1 n2 (UG.graph convGraph)

            -- create the conversion expressions
            case length edges >= 1 of
                True -> let expr = foldl1 inlineCExpr edges in
                    return $ trace' [MkSB expr]
                        (printf "Inlined Conversion expression %s => %s in Dim %s" u1 u2 "unknown")
                        expr
                False -> throwError $ printf "Cannot find conversion between units %s and %s" u1 u2




























