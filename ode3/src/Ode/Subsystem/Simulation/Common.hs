-----------------------------------------------------------------------------
--
-- Module      :  Subsystem.Simulation.Common
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  BSD3
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Code common to Interpreter and JITCompiler
--
-----------------------------------------------------------------------------

module Ode.Subsystem.Simulation.Common (
SimMetadata(..), writeMetadata
) where

import qualified Subsystem.SysState as Sys
import Text.Show.Pretty
import Text.Printf
-- put file output routines here, both
---- simulation output
---- simulation metadata output


data SimMetadata = SimMetadata
                    { statusCode    :: Int      -- return value from a simulation run
                    , stateNames    :: [String] -- unique names of all output state columns
                    , solver        :: Int      -- which solver was user
                    , simParams     :: Sys.SimParams -- basic simulation params
                    } deriving (Show)


-- | Writes metadata for a simulation run to a file
writeMetadata :: SimMetadata -> FilePath -> IO ()
writeMetadata simMetadata filename = writeFile filename outData
  where
    outData = printf "Metadata from simulation run - %s\n\n%s"
        (Sys._filename (simParams simMetadata)) (ppShow simMetadata)

