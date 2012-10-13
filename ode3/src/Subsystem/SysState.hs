-----------------------------------------------------------------------------
--
-- Module      :  UI.ShellState
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Main system state - inlcudes a nested set of records that hold all the compiler meta-data
-- We use fclabels to allow easy nested record access
-----------------------------------------------------------------------------

{-# LANGUAGE TypeOperators, GeneralizedNewtypeDeriving #-}

module Subsystem.SysState where
--
--(
--SysState(..), mkDefSysState, SimState(..), ModState(..), UnitsState(..),
--RepoSet,
--
---- useful labels and views
--lModState, vModEnv, vParsedFiles, vRepos
--) where

import Control.Category
import Data.Label as L
import Prelude hiding ((.), id)

import Control.Applicative
import Control.Monad
import Control.Monad.State as S
import Control.Monad.Error
import Control.Monad.Trans

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Bimap as Bimap


import qualified System.FilePath as FP
import qualified System.Directory as Dir

import AST.Common
import qualified AST.Module as MA
import qualified Utils.OrdSet as OrdSet
import qualified Subsystem.Units as U
import Utils.Utils

-- System State Monad --------------------------------------------------------------------------------------------------

--newtype SysExceptIO a = SysExceptIOC { runSysExceptIOC :: StateT SysState MExceptIO a }
--    deriving (Monad, MonadError String, MonadState SysState, MonadIO, Functor, Applicative)

type SysExceptIO = StateT SysState MExceptIO
type SysExcept = StateT SysState MExcept

-- extract and run the state
runSysExceptIO :: SysExceptIO a -> SysState -> MExceptIO (a, SysState)
runSysExceptIO m st = runStateT m st

mkSysExceptIO :: SysExcept a  -> SysExceptIO a
mkSysExceptIO m = do
    st <- S.get
    case (runStateT m st) of
        Left err -> throwError err
        Right (a, st') -> put st' >> return a

liftExSys :: MExcept a -> SysExceptIO a
liftExSys = lift . mkExceptIO

data SysState = SysState
    { _debug :: Bool                -- do we enable debug mode
    , _disableUnits :: Bool         -- disable type-checker units?
    , _simParams :: SimParams         -- simulation params
    , _modState :: ModState
    , _unitsState :: UnitsState
    } deriving Show


defSysState = SysState
    { _debug = False
    , _disableUnits = False
    , _simParams = defSimParams
    , _modState = defModState
    , _unitsState = defUnitsState
    }


-- Simulation Datatypes
data OdeSolver = FEuler | RK4 deriving (Show, Eq)
data OdeBackend = Interpreter | JITCompiler deriving (Show, Eq)

data SimParams = SimParams
    { _startTime :: Double
    , _endTime :: Double
    , _timestep :: Double               -- simulation timestep
    , _outputPeriod :: Integer          -- period with which to save simulation state to outfile, wrt timestep
    , _filename :: FilePath             -- output filename to save data to
    , _solver :: OdeSolver
    , _backend :: OdeBackend
    } deriving Show

defSimParams = SimParams
    { _startTime = 0
    , _endTime = 60
    , _timestep = 0.001                 -- 1ms
    , _outputPeriod = 500               -- 0.5s
    , _filename = "output.bin"          -- default output file
    , _solver = FEuler                  -- default solver
    , _backend = Interpreter            -- default backend
    }

-- | Holds the ordered set of enabled repositories
type RepoSet = OrdSet.OrdSet FilePath

data ModState = ModState
    { _repos :: RepoSet                 -- list of enabled module repositories
    , _modEnv :: MA.GlobalModEnv        -- map of loaded modules
    , _parsedFiles :: Set.Set ModRoot   -- set of fully parsed files
    , _replFile :: MA.FileData          -- a speical file that holds the REPL mod data
    } deriving Show

defModState = ModState
    { _repos = OrdSet.empty             -- do we add the defaults here?
    , _modEnv = Map.empty               -- map of loaded modules
    , _parsedFiles = Set.empty
    , _replFile = MA.mkFileData MA.replModRoot
    }


data UnitsState = UnitsState
    { _quantities :: U.QuantityBimap    -- a bimap of all quantities <-> dimension
--    , _unitAliases :: U.UnitAliasBimap  -- a bimap from unit <-> alias
    , _unitDimEnv :: U.UnitDimEnv       -- mapping from unit -> dimension
    , _convEnv :: U.ConvEnv             -- a mapping from dimension -> conv graph
    } deriving Show

defUnitsState = UnitsState
    { _quantities = U.defQuantities
--    , _unitAliases = Bimap.empty
    , _unitDimEnv = U.defUnits
    , _convEnv = U.defConvs
    }

-- TH splice
-- $(mkLabels [''SysState, ''SimState, ''ModState, ''UnitsState])
-- $(mkLabelsWith mkLabelName [''SysState, ''SimParams, ''ModState, ''UnitsState])
-- post TH, all generated defs/labels are in scope

-- We can't use TH splice above, as TH invokes ghci which results in undefined symbols for LLVM bindings
-- (main issue is ghci custom RTS linker is browkn r.e. weak symbols & C++ constructors/destructors
--   instead wait for ghci dynamic version that will use system linker & dlopen - GHC bug #3658)
-- Alt. generated splices (using Cog pre-processor)
{--[[[cog
import cog
def genLens(recName, fields):
  cog.outl("\n-- {0}".format(recName))
  for field in fields:
    lName = "l" + field[1].upper() + field[2:]
    cog.outl("{0} = lens ({1}) (\\x rec -> rec {{ {1} = x }})".format(lName, field))

genLens("SysState", ['_debug', '_disableUnits', '_simParams', '_modState', '_unitsState'])
genLens("SimParams", ['_startTime', '_endTime', '_timestep', '_outputPeriod', '_filename', '_solver', '_backend'])
genLens("ModState", ['_repos', '_modEnv', '_parsedFiles', '_replFile'])
genLens("UnitsState", ['_quantities', '_unitDimEnv', '_convEnv'])

]]]--}

-- SysState
lDebug = lens (_debug) (\x rec -> rec { _debug = x })
lDisableUnits = lens (_disableUnits) (\x rec -> rec { _disableUnits = x })
lSimParams = lens (_simParams) (\x rec -> rec { _simParams = x })
lModState = lens (_modState) (\x rec -> rec { _modState = x })
lUnitsState = lens (_unitsState) (\x rec -> rec { _unitsState = x })

-- SimParams
lStartTime = lens (_startTime) (\x rec -> rec { _startTime = x })
lEndTime = lens (_endTime) (\x rec -> rec { _endTime = x })
lTimestep = lens (_timestep) (\x rec -> rec { _timestep = x })
lOutputPeriod = lens (_outputPeriod) (\x rec -> rec { _outputPeriod = x })
lFilename = lens (_filename) (\x rec -> rec { _filename = x })
lSolver = lens (_solver) (\x rec -> rec { _solver = x })
lBackend = lens (_backend) (\x rec -> rec { _backend = x })

-- ModState
lRepos = lens (_repos) (\x rec -> rec { _repos = x })
lModEnv = lens (_modEnv) (\x rec -> rec { _modEnv = x })
lParsedFiles = lens (_parsedFiles) (\x rec -> rec { _parsedFiles = x })
lReplFile = lens (_replFile) (\x rec -> rec { _replFile = x })

-- UnitsState
lQuantities = lens (_quantities) (\x rec -> rec { _quantities = x })
lUnitDimEnv = lens (_unitDimEnv) (\x rec -> rec { _unitDimEnv = x })
lConvEnv = lens (_convEnv) (\x rec -> rec { _convEnv = x })
--[[[end]]] (checksum: 90c3e38b15e686fc6d9b39e59b7cb6cf)

-- a few useful views from top SysState into nested labels
vModEnv :: SysState :-> MA.GlobalModEnv
vModEnv = lModEnv . lModState

vParsedFiles :: SysState :-> Set.Set ModRoot
vParsedFiles = lParsedFiles . lModState

vRepos :: SysState :-> RepoSet
vRepos = lRepos . lModState

vLocalFile :: SysState :-> MA.FileData
vLocalFile = lReplFile . lModState

vQuantities :: SysState :-> U.QuantityBimap
vQuantities = lQuantities . lUnitsState

vUnitDimEnv :: SysState :-> U.UnitDimEnv
vUnitDimEnv = lUnitDimEnv . lUnitsState

vConvEnv :: SysState :-> U.ConvEnv
vConvEnv = lConvEnv . lUnitsState

-- helper func to modify records directly within SysState/SysExcept monad
getSysState l = L.get l <$> S.get
putSysState l a = (L.set l a <$> S.get) >>= put
modSysState l f = S.modify (\st -> L.modify l f st)

modSysStateM l f = do
    x <- getSysState l
    x' <- f x
    putSysState l x'


-- need to take out of IOdef
defRepos :: IO RepoSet
defRepos = repos
  where
    repos = liftM OrdSet.fromList $ sequence [curDir, userDir]
    curDir = Dir.getCurrentDirectory
    userDir = FP.combine <$> Dir.getAppUserDataDirectory "ode" <*> pure "repos"
    sysDir = undefined

-- | Sensible default values for initial system state
mkDefSysState :: IO SysState
mkDefSysState = do
    repos <- defRepos
    -- return $ defSysState { _modState = (_modState defSysState) {_repos = repos } }
    return $ set (lRepos . lModState) repos defSysState


-- | Helper function to clear the state of all loaded modules, types, units, etc.
clearModState :: SysState -> SysState
clearModState st = st'
  where
    repos = L.get vRepos st
    -- reset the module and units state to default, and add back the repos list
    st' = L.set lModState defModState st |> L.set lUnitsState defUnitsState |> L.set vRepos repos

-- testS :: SysState
-- testS =
