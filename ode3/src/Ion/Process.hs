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
processIon, matrixToTable
) where

import Ion.AST
import Utils.CommonImports

import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified Data.Graph.Inductive as G
import qualified Data.Graph.Inductive.NodeMap as NM
import Data.Graph.Inductive.Tree
import qualified Data.Graph.Inductive.Basic as GB
import qualified Utils.Graph as UG
import Data.Graph.Inductive.Query.DFS(noComponents)
import qualified Data.Array as A


-- Process Entry -------------------------------------------------------------------------------------------------------

processIon :: IonModel -> MExcept IonModel
processIon im = do
    trace' [MkSB im] "Input Ion AST" $ return ()
    mapM processChannel im
  where
    processChannel :: IonChannel -> MExcept IonChannel
    processChannel ionChan@IonChannel{..} = do
        ionChan' <- buildIonData ionChan >>= validateIonChan
        if subunits > 1
          then expandSubunits ionChan >>= buildIonData -- expand the reactions, and rebuild the data
          else return ionChan'

-- | Build the auxilary ion channel data structures, i.e. set of reactions and the reaction graph
buildIonData :: IonChannel -> MExcept IonChannel
buildIonData ionChan@IonChannel{..} = do
    return $ stocMatrix' (ionChan { states=states', transitionGraph=transitionGraph' })
  where
    states' = foldl addStates Set.empty transitions
    addStates states (Transition sA sB _ _) = Set.insert sA states |> Set.insert sB

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
    -- check initial state is valid
    unless (checkStateExists initialState)
        (throwError $ printf "(ION01) %s state is not contained in any reaction" (show initialState))
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
genStocMatrix :: IonChannel -> StocMatrix
genStocMatrix ionChan@IonChannel{..} = initArray A.// stocElems
  where
    initArray = A.listArray ((1,1),(length transitions, Set.size states)) [0,0..] -- create a zero-filled init array, (transitions x states)
    stateMap = Map.fromList $ zip (Set.toList states) [1..] -- mapping from a state to it's col in the stocMatrix
    -- list of updates to the inital array
    stocElems = concat $ map createElems (zip [1..] transitions)
    -- creates 2 inserts into array to represetn the state changes by the transition
    createElems (transIdx, (Transition a b _ _)) = [((transIdx, stateMap Map.! a), -1), ((transIdx, stateMap Map.! b), 1)]

-- Create a 2D representation of the array for printinf (taken from http://stackoverflow.com/questions/8901252/2d-array-in-haskell)
matrixToTable :: StocMatrix -> String
matrixToTable arr =
  unlines $ map (unwords . map (show . (arr A.!))) indices
  where indices = [[(x, y) | x <- [startX..endX]] | y <- [startY..endY]]
        ((startX, startY), (endX, endY)) = A.bounds arr
