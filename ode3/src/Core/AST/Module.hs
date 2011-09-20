-----------------------------------------------------------------------------
--
-- Module      :  Core.AST.Module
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Module system, represents either a closed, complete module or an open, parameterised model
--
-----------------------------------------------------------------------------

{-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures #-}

module Core.AST.Module (

) where

import qualified Data.Map as Map
import qualified Data.Bimap as Bimap
import Core.AST.Model
import Core.AST.Expr

-- | bidirectional map between internal ids and source ids
type IdBimap = Bimap.Bimap UntypedId SrcId

data Module = Module {model :: OrdModel TypedId, signature :: Map.Map SrcId Type, topIdsBimap :: IdBimap, lastUsedId :: UntypedId  }


createModule :: ModelMap TypedId -> IdBimap -> Module
createModule model idBimap =
