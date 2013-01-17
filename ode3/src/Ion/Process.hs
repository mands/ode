-----------------------------------------------------------------------------
--
-- Module      :  Process.Ion
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Set of procedures to validate an ion channel model, and obtain the stoc-matrix if required
--
-----------------------------------------------------------------------------

module Ion.Process (
processIon, matrixToTable,
getDetElems, getStocElems
) where

import Ion.AST
import Utils.CommonImports

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe(fromJust)
import qualified Data.Graph.Inductive as G
import qualified Data.Graph.Inductive.NodeMap as NM
import Data.Graph.Inductive.Tree
import qualified Data.Graph.Inductive.Basic as GB
import qualified Utils.Graph as UG
import Data.Graph.Inductive.Query.DFS(noComponents)
import qualified Data.Array as A
import Data.Monoid

-- Process Entry -------------------------------------------------------------------------------------------------------

processIon :: IonModel -> MExcept IonModel
processIon im = do
    trace' [MkSB im] "Input Ion AST" $ return ()
    mapM processChannel im
  where
    processChannel :: IonChannel -> MExcept IonChannel
    processChannel ionChan@IonChannel{..} = do
        ionChan' <- buildIonData ionChan >>= validateIonChan
        return ionChan'
        -- subunit handling
--        if subunits > 1
--          then expandSubunits ionChan >>= buildIonData -- expand the reactions, and rebuild the data
--          else return ionChan'

-- | Build the auxilary ion channel data structures, i.e. set of reactions and the reaction graph
buildIonData :: IonChannel -> MExcept IonChannel
buildIonData ionChan@IonChannel{..} = do
    return ((ionChan { states=states', transitionGraph=transitionGraph' }) |> stocMatrix')
  where
    -- states are defined by initital state list
    states' = Set.fromList . map fst $ initialStates

    transitionGraph' = foldl addTrans UG.mkGraphMap transitions
    addTrans gm (Transition sA sB fRate rRate) = UG.runGraph_ gm $ do
        _ <- UG.insertNodeM_ sA
        _ <- UG.insertNodeM_ sB
        NM.insMapEdgeM (sA, sB, fRate)
        NM.insMapEdgeM (sB, sA, rRate)

    stocMatrix' ionChan = ionChan { stocMatrix=(Just $ genStocMatrix ionChan) }

-- | Takes an Ion AST, and performs basic sanity checks on it, including
-- * do list of rates form a closed set
-- * are all state reactions between different reactions
-- * do open and initial states reference actualy existing states
validateIonChan :: IonChannel -> MExcept IonChannel
validateIonChan ionChan@IonChannel{..} = do
    -- check initial states are uniq
    unless (listUniqs . map fst $ initialStates)
        (throwError $ printf "(ION01) Repeated state values found in initial state definition")

    -- check all states in transitions exist
    mapM_ checkTransitionState transitions

    -- check open states are valid
    unless (all checkStateExists openStates)
        (throwError $ printf "(ION02) An open state found that is not contained in any reaction")

    -- check reactions form a single graph - i.e. 1 component
    let numComponents = noComponents $ UG.graph transitionGraph
    trace' [] (printf "Num components - %d" numComponents) $ return ()
    unless (numComponents == 1)
        (throwError $ printf "(ION03) The set of reactions is invalid, it contains %d sets of independent reactions" numComponents)

    -- check reactions have no loops (in terms of direct loops)
    unless (GB.isSimple $ UG.graph transitionGraph)
        (throwError $ printf "(ION04) The set of reactions is invalid, it contains loops into the same state")
    return ionChan
  where
    checkStateExists s = Set.member s states

    checkTransitionState t@(Transition sA sB _ _) = unless (checkStateExists sA && checkStateExists sB)
        (throwError $ printf "(ION02) Transition %s references undefined state" (show t))

-- Subunit handling ----------------------------------------------------------------------------------------------------
-- | NYI - This fucntions expands the set of reactions, initial and open states to accomodate n-identical subunits
expandSubunits :: IonChannel -> MExcept IonChannel
expandSubunits ionChan@IonChannel{..} = do
    return ionChan
  where
-- basic algorithm - very slow, prob exp-O notation as requries brute-force check over entire expanded state space for suitable reactions, anyway,
-- first create the new set of states (binomial coef - (x+n-1, n)) and insert into a graph
-- then scan over all possible combinations of nodes
-- * if the state diff is only 1, then determine if a link exists in the intial reaction graph
-- * add the new link, having adjusted the fwd and rev transition rates


-- Stoc Matrix Generation ----------------------------------------------------------------------------------------------
genStocMatrix :: IonChannel -> IonMatrix
genStocMatrix ionChan@IonChannel{..} = initArray A.// stocElems
  where
    initArray = initMat (Set.size states) (length transitions) (Num 0) -- zero-filled init array, (states x transitions)
    stateMap = Map.fromList $ zip (Set.toList states) [1..] -- mapping from a state to it's row in the stocMatrix
    -- list of updates to the inital array
    stocElems = concat $ map createElems (zip [1..] transitions)
    -- creates 2 inserts into array to represetn the state changes by the transition
    createElems (transIdx, (Transition a b _ _)) = [((stateMap Map.! a, transIdx), Num (-1)), ((stateMap Map.! b, transIdx), Num 1)]

-- Create a 2D representation of the array for printinf (taken from http://stackoverflow.com/questions/8901252/2d-array-in-haskell)
matrixToTable :: IonMatrix -> String
matrixToTable arr = unlines $ map (unwords . map (printElem . (arr A.!))) indices
  where
    indices = [[(row, col) | row <- [startRow..endRow]] | col <- [startCol..endCol]]
    ((startRow, startCol), (endRow, endCol)) = A.bounds arr
    printElem (Num n) | n == 1   = "+1"
    printElem (Num n) | n == -1  = "-1"
    printElem _                 = " 0"


-- Deterministic Matrix Generation -------------------------------------------------------------------------------------
-- fold over the graph nodes (i.e. states), building up an ion expression for each one based upon the node in/out edges
getDetElems :: IonChannel -> [IonExpr]
getDetElems ionChan@IonChannel{..} = map (optExpr . calcDetExpr) (Set.toList states)
  where
    g = UG.graph transitionGraph

    -- build an expr based on the in/out edges, we unroll the matrix mult here for simpliticy
    calcDetExpr :: Id -> IonExpr
    calcDetExpr state = Add outgoingEdges ingoingEdges
      where
        nodeId = fromJust $ UG.getNodeInt transitionGraph state
        -- outgoing edges from node are neg, ingoing edges are pos
        outgoingEdges = Neg $ Mul (Var state) (getProduct . mconcat . map prodEdges $ G.lsuc g nodeId)
        prodEdges (_, rateExpr) = Product rateExpr

        ingoingEdges = getSum . mconcat . map sumEdges $ G.lpre g nodeId
        sumEdges (preNodeId, rateExpr) = Sum $ Mul (Var . fromJust $ G.lab g preNodeId) rateExpr


-- Stochastic Matrix Generation -------------------------------------------------------------------------------------
-- fold over the transition list, generating the weiner exprs as go along
getStocElems :: IonChannel -> [IonExpr]
getStocElems ionChan@IonChannel{..} = map optExpr $ A.elems (matVecMult eMulF weinerVec)
  where
    -- a 2D array represeting the diag matrix F
    fDiagMat = genDiagMat (Num 0) (map genProp transitions)
    genProp Transition{..} = Sqrt $ Add (Mul (Var stateA) fRate) (Mul (Var stateB) rRate)
    -- e.F(x) matrix - matrix mult
    eMulF = matMatMult (fromJust stocMatrix) fDiagMat
    -- a dummy array for representing the weiner for each transition (we actually require them per state)
    weinerVec = A.listArray (1, length transitions) $ repeat (Num 1)
