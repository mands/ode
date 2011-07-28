-----------------------------------------------------------------------------
--
-- Module      :  Core.Renamer
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- |Renamer takes a reordered AST and renames all variables into unique values, thus it also deals with all scoping
-- issues. As oly are two scopes this should be fairly easy.
-- It also checks for all vlaue declarations, unused values, undefined values, and so on
-- Can issue errors due to user defined model
--
-----------------------------------------------------------------------------

module Core.Renamer (
rename
) where

import qualified Data.Map as Map
import qualified Data.Foldable as DF
import qualified Data.Traversable as DT
import qualified Core.AST as C
import Data.Maybe (fromJust)
import Utils.Utils
import Utils.MonadSupply

-- we need a supply of uniques, use monad supply again but with user-start param
type UniqueIntSupply = Supply Int

-- main types
type TopBinds = Map.Map C.Id Int
type LocalBinds = Map.Map C.Id Int

-- |Main rename function, takes a model bound by Ids and returns a single-scoped model bound by unique ints
-- I don't think this function can ever fail
-- eventually will take/return the higest bound int within the model
rename :: C.Model C.Id -> MExcept (C.OrdModel Int)
rename cModel = Right m'
  where

    -- try build a quick ordered model using C.Id
    oModel :: C.OrdModel C.Id
    oModel = C.fromList $ C.toList cModel

    -- dummy model consisting of just 1
    m' = C.fold mInsert C.empty oModel
    mInsert :: C.Top C.Id -> C.OrdModel Int -> C.OrdModel Int
    mInsert v m = C.insert 1 (fmap (\_ -> 1) v) m

    -- then map over each expr, using the topmap, converting lets and building a new scopemap
    -- as traversing expr, as order is fixed this should be ok


-- |Need to build a conversion map of the top values first
procTopBinds :: C.OrdModel C.Id -> TopBinds
procTopBinds model = undefined -- topBinds
  where
    --(topBinds, uniqs) = fromJust $ runSupply supplyMap [1..]
    --supplyMap = C.fold convTopBind Map.empty model
    --convTopBind (C.TopLet b _) m = do
    --    b' <- supply
    --    return Map.insert b' b m

























