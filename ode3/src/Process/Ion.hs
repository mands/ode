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

module Process.Ion (
processIon
) where

import AST.Ion
import Utils.CommonImports

import qualified Data.Set as Set

import qualified Data.Graph.Inductive as G
import qualified Data.Graph.Inductive.NodeMap as NM
import Data.Graph.Inductive.Tree
import qualified Data.Graph.Inductive.Basic as GB
import qualified Utils.Graph as UG
import Data.Graph.Inductive.Query.DFS(noComponents)

processIon :: IonModel -> MExcept IonModel
processIon im = do
    trace' [MkSB im] "Input Ion AST" $ return ()

    im' <- mapM buildIonData im
    mapM_ validateIon im'
    return im'



buildIonData :: IonChannel -> MExcept IonChannel
buildIonData ionChan@IonChannel{..} = do

    return $ ionChan { species=species', reactionGraph=reactionGraph' }
  where
    species' = foldl addSpecies Set.empty states
    addSpecies speciesSet (StateReaction sA sB _ _) = Set.insert sA speciesSet |> Set.insert sB

    reactionGraph' = foldl addReaction UG.mkGraphMap states
    addReaction gm (StateReaction sA sB fRate rRate) = UG.runGraph_ gm $ do
        _ <- UG.insertNodeM_ sA
        _ <- UG.insertNodeM_ sB
        NM.insMapEdgeM (sA, sB, fRate)
        NM.insMapEdgeM (sB, sA, rRate)


-- | Takes an Ion AST, and performs basic sanity checks on it, including
-- * do list of rates form a closed set
-- * are all state reactions between different reactions
-- * do open and initial states reference actualy existing states
validateIon :: IonChannel -> MExcept ()
validateIon ionChan@IonChannel{..} = do
    -- check initial state is valid
    unless (checkStateExists initialState)
        (throwError $ printf "(ION01) %s state is not contained in any reaction" (show initialState))
    -- check open states are valid
    unless (all checkStateExists openStates)
        (throwError $ printf "(ION02) An open state found that is not contained in any reaction")

    -- check reactions form a single graph - i.e. 1 component
    let numComponents = noComponents $ UG.graph reactionGraph
    trace' [] (printf "Num components - %d" numComponents) $ return ()
    unless (numComponents == 1)
        (throwError $ printf "(ION03) The set of reactions is invalid, it contains %d sets of independent reactions" numComponents)

    -- check reactions have no loops (in terms of direct loops)
    unless (GB.isSimple $ UG.graph reactionGraph)
        (throwError $ printf "(ION04) The set of reactions is invalid, it contains loops into the same state")

    return ()
  where
    checkStateExists s = Set.member s species
