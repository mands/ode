-----------------------------------------------------------------------------
--
-- Module      :  Process.Ion
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  BSD3
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
getDetElems, getStocElems, getInitialVals
) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Traversable as DT
import qualified Data.Array as A
import Data.Monoid
import Data.Maybe(fromJust)

import qualified Data.Graph.Inductive as G
import qualified Data.Graph.Inductive.NodeMap as NM
import Data.Graph.Inductive.Tree
import qualified Data.Graph.Inductive.Basic as GB
import Data.Graph.Inductive.Query.DFS(noComponents)

import Ion.AST
import Ode.Utils.CommonImports
import qualified Ode.Utils.Graph as UG
import qualified Ode.Utils.OrdMap as OrdMap
import AST.CoreFlat(SimType(..))

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
    return ((ionChan { states=states', transitionGraph=transitionGraph', transitions=transitions', wieners=(Just wieners'), vals=vals' }) |> stocMatrix')
  where
    -- states are defined by initital state list
    states' = Set.fromList . map fst $ initialStates

    transitionGraph' = foldl addTrans UG.mkGraphMap transitions'
    addTrans gm (Transition sA sB fRate rRate) = UG.runGraph_ gm $ do
        _ <- UG.insertNodeM_ sA
        _ <- UG.insertNodeM_ sB
        NM.insMapEdgeM (sA, sB, fRate)
        NM.insMapEdgeM (sB, sA, rRate)

    stocMatrix' ionChan = ionChan { stocMatrix=(Just $ genStocMatrix ionChan) }

    -- wiener gen
    wieners' = take (length transitions) $ genUniqs "_w"

    -- tmp val gen - bit hacky but fuck it
    ((_, vals'), transitions') = DT.mapAccumL genTmpRateVals (genUniqs "_rate", vals) transitions
      where
        genTmpRateVals ((idA : idB : ids), vals) (Transition sA sB fRateExpr rRateExpr) =
            ((ids, OrdMap.insert idA fRateExpr vals |> OrdMap.insert idB rRateExpr), Transition sA sB (Var idA) (Var idB))


-- | Takes an Ion AST, and performs basic sanity checks on it, including
-- * do list of rates form a closed set
-- * are all state reactions between different reactions
-- * do open and initial states reference actualy existing states
validateIonChan :: IonChannel -> MExcept IonChannel
validateIonChan ionChan@IonChannel{..} = do
    -- check initial states are uniq
    unless (listUniqs . map fst $ initialStates)
        (throwError $ printf "(ION01) Repeated state values found in initial state definition")

    -- check initial vals between 0 >= x <= 1, and all sum to 1
    unless (stateBoundaries)
        (throwError $ printf "(IONXX) Initial values are not between 0 and 1 or sum to 1 with sufficent precision (%g)" epsilon)

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

    -- epsilon indiactes the max error when setting final initial val as remainder of 1 - (sum other initial vals)
    epsilon = 1e-9
    stateBoundaries = maximum stateVals <= 1 && minimum stateVals >= 0 && abs (1 - sum stateVals) < epsilon
    stateVals = map snd $ initialStates

-- Setup Initial Values ----------------------------------------------------------------------------

-- | Generate the initial values based on ODE/SDE (prop based) or SSA (whole number based)
getInitialVals :: IonChannel -> [(Id, Double)]
getInitialVals ionChan@IonChannel{..} =
    case simType of
        SimRRE  -> rreStates ++ [(finId, rreFinVal)]
        -- ODE / SDE
        _       -> odeStates ++ [(finId, odeFinVal)]
  where
    (finId, finVal) = last initialStates

    -- ODE / SDE form, final val = 1 - (sum other initial vals)
    odeStates = init $ initialStates
    odeFinVal = 1 - (sum . map snd $ odeStates)

    -- RRE form, round the vals - this should be fine as round-even mode used, however can be subverted by careful use of vals under epsilon
    -- TODO - need to add check that vals = density
    rreStates = map (\(i,v) -> (i, roundDouble $ density * v)) . init $ initialStates
    rreFinVal = max (density - (sum . map snd $ rreStates)) 0

    -- round a double to the nearest integer (valid for n < 2**52)
    roundDouble :: Double -> Double
    roundDouble d = fromIntegral $ round d


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
getDetElems ionChan@IonChannel{..} = map (optExpr . calcDetExpr) $ Set.toList states
  where
    g = UG.graph transitionGraph

    -- build an expr based on the in/out edges, we unroll the matrix mult here for simpliticy
    calcDetExpr :: Id -> IonExpr
    calcDetExpr state = Add outgoingEdges ingoingEdges
      where
        nodeId = fromJust $ UG.getNodeInt transitionGraph state
        -- outgoing edges from node are neg, ingoing edges are pos
        outgoingEdges = Mul (Neg (getSum . mconcat . map prodEdges $ G.lsuc g nodeId)) (Var state)
        prodEdges (_, rateExpr) = Sum rateExpr

        ingoingEdges = getSum . mconcat . map sumEdges $ G.lpre g nodeId
        sumEdges (preNodeId, rateExpr) = Sum $ Mul rateExpr (Var . fromJust $ G.lab g preNodeId)


-- Stochastic Matrix Generation -------------------------------------------------------------------------------------
-- fold over the transition list, generating the wiener exprs as go along
getStocElems :: IonChannel -> ([IonExpr], OrdMap.OrdMap Id IonExpr)
getStocElems ionChan@IonChannel{..} = (map optExpr $ A.elems (matVecMult eMulF wienerVec), tmpVals)
  where
    -- a 2D array represeting the diag matrix F
    fDiagMat = genDiagMat (Num 0) transitions'

    -- obtain the tmpVals from the struc
    ((_, tmpVals), transitions') = DT.mapAccumL genProp (genUniqs "_prop", OrdMap.empty) transitions
    genProp ((propId : ids), tmpVals) Transition{..} = ((ids, OrdMap.insert propId propExpr tmpVals), Var propId)
      where
        propExpr = Sqrt $ Add (Mul (Var stateA) fRate) (Mul (Var stateB) rRate)

    -- e.F(x) matrix - matrix mult
    eMulF = matMatMult (fromJust stocMatrix) fDiagMat
    -- a dummy array for representing the wiener for each transition (we actually require them per state)
    wienerVec = A.listArray (1, length transitions) $ map (Var) (fromJust wieners)

-- Helper functions ----------------------------------------------------------------------------------------------------

-- | Generate a inf-list of unique ids using the given prefix
genUniqs :: String -> [String]
genUniqs prefix = map (\i -> prefix ++ (show i)) [1..]
