-----------------------------------------------------------------------------
--
-- Module      :  Common.AST
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | common AST Types, ADTs, etc.
--
-----------------------------------------------------------------------------

module Lang.Common.AST (
    NumTy, UntypedId, Id,
    SrcId, UnitT, DesId
) where

-- | an individual number type, not sure if needed, used to convert from double to integer
type NumTy = Double

type UnitT = String

-- TODO - change to newtype?
-- | Identifier - basicially RdrName - needs to become parameterised, hold the id and the optional unit annotation
type SrcId = String
type UntypedId = Int
type Id = UntypedId
type DesId = (SrcId, Maybe UnitT)



