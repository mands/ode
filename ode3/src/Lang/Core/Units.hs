-----------------------------------------------------------------------------
--
-- Module      :  Lang.Core.Units
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Units AST and checking
--
-----------------------------------------------------------------------------

{-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures, DataKinds, DeriveFunctor #-}

module Lang.Core.Units (
    Quantity, Quantities, QuantityBimap,
    DimVec(..), addDim, subDim, mkDimVec, dimensionless, isZeroDim,
    UnitDef(..), Unit, mkUnit, UnitDimEnv, SrcUnit,
    CExpr(..), COp(..), ConvDef(..), ConvEnv,

    addQuantitiesToBimap, addUnitsToEnv, addConvsToGraph
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.State (runState)
import Text.Printf (printf)

import qualified Data.Graph.Inductive as G
import qualified Data.Graph.Inductive.NodeMap as NM
import qualified Data.Graph.Inductive.Query.BFS as BFS

import qualified Data.Foldable as DF
import qualified Data.List as List
import qualified Data.List.Split as ListSplit
import qualified Data.Bimap as Bimap
import qualified Data.Map as Map
import Data.Monoid

import Utils.Utils
import qualified Utils.Graph as UG

-- Dimensions ----------------------------------------------------------------------------------------------------------

-- Dimension Vector
-- Dim L M T I O J N
data DimVec = DimVec    { dimL :: Integer, dimM :: Integer, dimT :: Integer, dimI :: Integer
                        , dimO :: Integer, dimJ :: Integer, dimN :: Integer
                        } deriving (Show, Eq, Ord)

mkDimVec = DimVec 0 0 0 0 0 0 0
dimensionless = DimVec 0 0 0 0 0 0 0

-- Dim helper funcs
addDim (DimVec l m t i o j n) (DimVec l' m' t' i' o' j' n') =
    DimVec (l+l') (m+m') (t+t') (i+i') (o+o') (j+j') (n+n')

subDim (DimVec l m t i o j n) (DimVec l' m' t' i' o' j' n') =
    DimVec (l-l') (m-m') (t-t') (i-i') (o-o') (j-j') (n-n')

mulDim (DimVec l m t i o j n) x = DimVec (l*x) (m*x) (t*x) (i*x) (o*x) (j*x) (n*x)

isZeroDim = (==) dimensionless

negDim (DimVec l m t i o j n) = DimVec (negate l) (negate m) (negate t) (negate i) (negate o) (negate j) (negate n)

instance Monoid DimVec where
    mempty = dimensionless
    mappend = addDim


-- Quantities ----------------------------------------------------------------------------------------------------------
type Quantity = String
type QuantityBimap = Bimap.Bimap Quantity DimVec
type Quantities = [(Quantity, DimVec)]

-- Quantity helper funcs
addQuantitiesToBimap :: QuantityBimap -> Quantities -> QuantityBimap
addQuantitiesToBimap = foldl (\qBimap (quantity, dimVec) -> Bimap.insert quantity dimVec qBimap)

-- Units ---------------------------------------------------------------------------------------------------------------
newtype Unit = UnitC [(String, Integer)] deriving (Eq, Ord)

instance Show Unit where
    show (UnitC units) = List.intercalate "." (map showUnit units)
      where
        showUnit (baseName, index) = baseName ++ show index

-- Unit helper funcs

-- need to filter dups, process indices, and define ordering
mkUnit :: SrcUnit -> Unit
mkUnit base@[(baseName, 1) ]= UnitC base
mkUnit units = UnitC . Map.toList . foldl mkUnit' Map.empty $ units
  where
    mkUnit' unitMap (unitName, index) = Map.insertWith' (+) unitName index unitMap

isBaseUnit :: Unit -> Bool
isBaseUnit (UnitC [(baseName, 1)]) = True
isBaseUnit _ = False

type SrcUnit = [(String, Integer)]

-- hold this temp structure in indiv module, and promote to global state (UnitDimEnv) when imported & processed
data UnitDef :: * where
    BaseUnitDef :: Unit -> DimVec -> UnitDef
    DerivedUnitDef :: Unit -> UnitDef
    deriving (Eq, Ord, Show)

-- type BaseUnitMap = Map.Map Unit Integer
--type UnitAlias = String
--type UnitAliasBimap = Bimap.Bimap UnitAlias Unit

-- TODO - can these structures be simplified/unified
-- mapping from (base?) units to dimensions
type UnitDimEnv = Map.Map Unit DimVec

-- Unit Env helper funcs
-- calculate the dimension of a given derived unit
calcUnitDim :: Unit -> UnitDimEnv -> MExcept DimVec
calcUnitDim u@(UnitC units) unitEnv = mconcat <$> mapM getDim units
  where
    getDim (name, index) = case Map.lookup (mkUnit [(name,1)]) unitEnv of
        Nothing -> throwError $ printf "Reference to unknown base unit %s found in %s" name (show u)
        Just dim -> return $ mulDim dim index

addUnitsToEnv :: UnitDimEnv -> [UnitDef] -> MExcept UnitDimEnv
addUnitsToEnv unitEnv units = DF.foldlM addUnit unitEnv units
  where
    addUnit unitEnv (BaseUnitDef u d) = case Map.lookup u unitEnv of
        Nothing -> return $ Map.insert u d unitEnv
        Just _ -> throwError $ printf "Base unit %s already defined" (show u)
    addUnit unitEnv (DerivedUnitDef u) = case Map.lookup u unitEnv of
        Nothing -> Map.insert u <$> (calcUnitDim u unitEnv) <*> pure unitEnv
        Just _ -> throwError $ printf "Derived unit %s already defined" (show u)

-- do the units exist, and are they the same dimensions
getDimForUnits :: Unit -> Unit -> UnitDimEnv -> MExcept DimVec
getDimForUnits u1 u2 uEnv = do
    dim1 <- calcUnitDim u1 uEnv
    dim2 <- calcUnitDim u2 uEnv
    if dim1 /= dim2 then
        throwError $ printf "Dimension mismatch - units %s (Dim %s) and %s (Dim %s)" (show u1) (show dim1) (show u2) (show dim2)
        else return dim1

-- Conversion ----------------------------------------------------------------------------------------------------------
data ConvDef = ConvDef Unit Unit CExpr deriving (Show, Eq, Ord)

-- main graph type, nodes :: Units, edges :: CExprs
type ConvGraph = UG.GraphMap Unit CExpr

--data ConvData = ConvData { nodeMap :: NM.NodeMap Unit, graph :: ConvGraph }
--mkConvData = ConvData NM.new G.empty

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
        dim <- getDimForUnits fromUnit toUnit unitEnv
        -- insert edge and nodes into graph
        let convData' = updateGraph (getConvGraph dim cEnv) convDef
        -- update the convEnv
        return $ Map.insert dim convData' cEnv

    -- add the unit nodes and the conversion edge between them
    updateGraph :: ConvGraph -> ConvDef -> ConvGraph
    updateGraph cData convDef@(ConvDef fromUnit toUnit cExpr) = UG.runGraph_ cData $ do
        _ <- NM.insMapNodeM fromUnit
        _ <- NM.insMapNodeM toUnit
        NM.insMapEdgeM (fromUnit, toUnit, cExpr)


-- | Calculate, if possible, the conversion expression to use between two units
-- need to get the graph, calc the path between the nodes, then inline the expression
calcConvExpr :: Unit -> Unit -> UnitDimEnv -> ConvEnv -> MExcept CExpr
-- TODO -- need to handle case of un-dimenstioned values explitictly here
calcConvExpr (UnitC []) toUnit uEnv cEnv = undefined

calcConvExpr fromUnit toUnit uEnv cEnv = do
    -- get the graph
    dim <- getDimForUnits fromUnit toUnit uEnv
    let convGraph = getConvGraph dim cEnv

    -- get the path
    n1 <- maybeToExcept (UG.getNodeInt convGraph fromUnit) $ printf "Unit %s not found in graph" (show fromUnit)
    n2 <- maybeToExcept (UG.getNodeInt convGraph toUnit) $ printf "Unit %s not found in graph" (show toUnit)
    let edges = UG.getEdgesFromPath $ BFS.lesp n1 n2 (UG.graph convGraph)

    case length edges >= 1 of
        True -> return $ foldl1 inlineCExpr edges
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

