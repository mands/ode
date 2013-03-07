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
    { _debug        :: Bool             -- do we enable debug mode
    , _simParams    :: SimParams        -- simulation params
    , _modState     :: ModState
    , _unitsState   :: UnitsState
    } deriving Show


defSysState = SysState
    { _debug        = False
    , _simParams    = defSimParams
    , _modState     = defModState
    , _unitsState   = defUnitsState
    }

-- Simulation Datatypes
data OdeSolver  = FEuler | RK4 | Adaptive deriving (Show, Eq)
data SdeSolver  = EM | ProjEM deriving (Show, Eq)
data OdeBackend = Interpreter | JITCompiler | AOTCompiler | ObjectFile deriving (Show, Eq)
data OdeLinker  = Static | Dynamic deriving (Show, Eq)
data OdeMathModel = Strict | Fast deriving (Show, Eq)
data OdeMathLib = GNU | AMD | Intel deriving (Show, Eq)
data OdeModelType = Stiff | NonStiff deriving (Show, Eq)

data SimParams = SimParams
    {
    -- timing params
      _startTime    :: Double
    , _stopTime     :: Double
    , _timestep     :: Double           -- simulation timestep

    -- adaptive solver params
    , _maxTimestep  :: Double
    , _maxNumSteps  :: Integer
    , _relError     :: Double
    , _absError     :: Double
    , _modelType    :: OdeModelType

    -- unit params
    , _unitsCheck   :: Bool             -- disable type-checker units?
    , _timeUnit     :: U.Unit           -- default unit for time parameter

    -- output params
    , _outputPeriod :: Double           -- period interval with which to save simulation state to outfile, should be multiple of timestep
    , _mStartOutput :: Maybe Double     -- time T1 at which to start output to datafile
    , _filename     :: FilePath         -- output filename to save data to

    -- compilation params
    , _odeSolver    :: OdeSolver
    , _sdeSolver    :: SdeSolver
    , _backend      :: OdeBackend
    , _linker       :: OdeLinker
    , _execute      :: Bool
    , _exeName      :: String

    -- fast math
    , _mathModel    :: OdeMathModel
    , _mathLib      :: OdeMathLib
    , _vecMath      :: Bool

    -- optimisations
    , _optimise     :: Bool
    , _optShortCircuit  :: Bool         -- do we perform short-circuit evaluation of booleans?
    , _optPowerExpan    :: Bool         -- do we perform expansion of calls to pow()?
    } deriving Show

defSimParams = SimParams
    { _startTime    = 0
    , _stopTime     = 60
    , _timestep     = 0.001             -- 1ms

    , _maxTimestep  = 1                 -- 1s
    , _maxNumSteps  = 500               -- CVODE default
    , _relError     = 1e-6              -- recommended by CVODE (0.1 of 0.1%)
    , _absError     = 1e-9              -- is model dependent, and should specific to each state val
    , _modelType    = Stiff             -- default model type for adaptive solver

    , _unitsCheck   = True              -- unit-checking enabled
    , _timeUnit     = U.uSeconds        -- time set to seconds

    , _outputPeriod = 0.5               -- 0.5s
    , _mStartOutput  = Nothing           -- start output immedietly
    , _filename     = "output.bin"      -- default output file

    , _odeSolver    = FEuler            -- default solver
    , _sdeSolver    = EM                -- default solver

    , _backend      = Interpreter       -- default backend
    , _linker       = Dynamic           -- always dyn link (not used for Interpreter & JIT)
    , _execute      = True              -- always execute (not used for Interpreter & JIT)
    , _exeName      = "Sim.exe"         -- executable name (only for aotcompilation output)

    , _mathModel    = Fast              -- always use fast maths during code-gen
    , _mathLib      = GNU               -- always use GNU libm
    , _vecMath      = False             -- vecMath optimisation disabled by default

    , _optimise     = True              -- always optimise during code-gen
    , _optShortCircuit  = True          -- always perform short-circuit evaluation
    , _optPowerExpan    = True          -- always perform expansion of calls to pow()

    }

-- | Holds the ordered set of enabled repositories
type RepoSet = OrdSet.OrdSet FilePath

data ModState = ModState
    { _repos    :: RepoSet                 -- list of enabled module repositories
    , _modEnv   :: MA.GlobalModEnv        -- map of loaded modules
    , _parsedFiles :: Set.Set ModRoot   -- set of fully parsed files
    , _replFile :: MA.FileData          -- a speical file that holds the REPL mod data
    } deriving Show

defModState = ModState
    { _repos    = OrdSet.empty             -- do we add the defaults here?
    , _modEnv   = Map.empty               -- map of loaded modules
    , _parsedFiles = Set.empty
    , _replFile = MA.mkFileData MA.replModRoot
    }


data UnitsState = UnitsState
    { _quantities   :: U.QuantityBimap      -- a bimap of all quantities <-> dimension
--    , _unitAliases :: U.UnitAliasBimap    -- a bimap from unit <-> alias
    , _unitDimEnv   :: U.UnitDimEnv         -- mapping from unit -> dimension
    , _derivedEnv   :: U.DerivedUnitEnv     -- mapping from derived unit -> base units
    , _convEnv      :: U.ConvEnv            -- a mapping from dimension -> conv graph
    } deriving Show

defUnitsState = UnitsState
    { _quantities   = U.defQuantities
--    , _unitAliases = Bimap.empty
    , _unitDimEnv   = U.defUnits
    , _derivedEnv   = Map.empty
    , _convEnv      = U.defConvs
    }

-- TH splice
-- $(mkLabels [''SysState, ''SimState, ''ModState, ''UnitsState])
-- $(mkLabelsWith mkLabelName [''SysState, ''SimParams, ''ModState, ''UnitsState])
-- post TH, all generated defs/labels are in scope

-- We can't use TH splice above, as TH invokes ghci which results in undefined symbols for LLVM bindings
-- (main issue is ghci custom RTS linker is broken wrt. weak symbols & C++ constructors/destructors
--   instead wait for ghci dynamic version that will use system linker & dlopen - GHC bug #3658)
-- Alt. generated splices (using Cog pre-processor)
{--[[[cog
import cog
def genLens(recName, fields):
  cog.outl("\n-- {0}".format(recName))
  for field in fields:
    lName = "l" + field[1].upper() + field[2:]
    cog.outl("{0} = lens ({1}) (\\x rec -> rec {{ {1} = x }})".format(lName, field))

genLens("SysState",     ['_debug', '_simParams', '_modState', '_unitsState'])
genLens("SimParams",    ['_startTime', '_stopTime', '_timestep', '_maxTimestep', '_maxNumSteps', '_relError', '_absError', '_modelType'
                        , '_unitsCheck', '_timeUnit', '_filename', '_outputPeriod', '_mStartOutput', '_odeSolver', '_sdeSolver', '_backend', '_linker', '_execute'
                        , '_exeName', '_mathModel', '_mathLib', '_vecMath', '_optimise', '_optShortCircuit', '_optPowerExpan'])
genLens("ModState",     ['_repos', '_modEnv', '_parsedFiles', '_replFile'])
genLens("UnitsState",   ['_quantities', '_unitDimEnv', '_derivedEnv', '_convEnv'])



]]]--}

-- SysState
lDebug = lens (_debug) (\x rec -> rec { _debug = x })
lSimParams = lens (_simParams) (\x rec -> rec { _simParams = x })
lModState = lens (_modState) (\x rec -> rec { _modState = x })
lUnitsState = lens (_unitsState) (\x rec -> rec { _unitsState = x })

-- SimParams
lStartTime = lens (_startTime) (\x rec -> rec { _startTime = x })
lStopTime = lens (_stopTime) (\x rec -> rec { _stopTime = x })
lTimestep = lens (_timestep) (\x rec -> rec { _timestep = x })
lMaxTimestep = lens (_maxTimestep) (\x rec -> rec { _maxTimestep = x })
lMaxNumSteps = lens (_maxNumSteps) (\x rec -> rec { _maxNumSteps = x })
lRelError = lens (_relError) (\x rec -> rec { _relError = x })
lAbsError = lens (_absError) (\x rec -> rec { _absError = x })
lModelType = lens (_modelType) (\x rec -> rec { _modelType = x })
lUnitsCheck = lens (_unitsCheck) (\x rec -> rec { _unitsCheck = x })
lTimeUnit = lens (_timeUnit) (\x rec -> rec { _timeUnit = x })
lFilename = lens (_filename) (\x rec -> rec { _filename = x })
lOutputPeriod = lens (_outputPeriod) (\x rec -> rec { _outputPeriod = x })
lMStartOutput = lens (_mStartOutput) (\x rec -> rec { _mStartOutput = x })
lOdeSolver = lens (_odeSolver) (\x rec -> rec { _odeSolver = x })
lSdeSolver = lens (_sdeSolver) (\x rec -> rec { _sdeSolver = x })
lBackend = lens (_backend) (\x rec -> rec { _backend = x })
lLinker = lens (_linker) (\x rec -> rec { _linker = x })
lExecute = lens (_execute) (\x rec -> rec { _execute = x })
lExeName = lens (_exeName) (\x rec -> rec { _exeName = x })
lMathModel = lens (_mathModel) (\x rec -> rec { _mathModel = x })
lMathLib = lens (_mathLib) (\x rec -> rec { _mathLib = x })
lVecMath = lens (_vecMath) (\x rec -> rec { _vecMath = x })
lOptimise = lens (_optimise) (\x rec -> rec { _optimise = x })
lOptShortCircuit = lens (_optShortCircuit) (\x rec -> rec { _optShortCircuit = x })
lOptPowerExpan = lens (_optPowerExpan) (\x rec -> rec { _optPowerExpan = x })

-- ModState
lRepos = lens (_repos) (\x rec -> rec { _repos = x })
lModEnv = lens (_modEnv) (\x rec -> rec { _modEnv = x })
lParsedFiles = lens (_parsedFiles) (\x rec -> rec { _parsedFiles = x })
lReplFile = lens (_replFile) (\x rec -> rec { _replFile = x })

-- UnitsState
lQuantities = lens (_quantities) (\x rec -> rec { _quantities = x })
lUnitDimEnv = lens (_unitDimEnv) (\x rec -> rec { _unitDimEnv = x })
lDerivedEnv = lens (_derivedEnv) (\x rec -> rec { _derivedEnv = x })
lConvEnv = lens (_convEnv) (\x rec -> rec { _convEnv = x })
--[[[end]]] (checksum: 1d4add76207dc6219d165882e3b5d19c)

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

vDerivedEnv :: SysState :-> U.DerivedUnitEnv
vDerivedEnv = lDerivedEnv . lUnitsState

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

-- Other Helper functions ----------------------------------------------------------------------------------------------
-- | Calculate the integer interval to output data for fixed timestep simulations
calcOutputInterval :: SimParams -> Integer
calcOutputInterval SimParams{..} = round $ _outputPeriod / _timestep -- can't used floor as rounds down

-- | adjust stop time to account for period, round up to next mult of period
calcAdjustedStopTime :: SimParams -> Double
calcAdjustedStopTime SimParams{..} = (fromIntegral . ceiling $ _stopTime / _outputPeriod) * _outputPeriod
