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

{-# LANGUAGE TemplateHaskell, TypeOperators, GeneralizedNewtypeDeriving #-}

module UI.SysState where
--
--(
--SysState(..), mkDefSysState, SimState(..), ModState(..), UnitsState(..),
--RepoSet,
--
---- useful labels and views
--lModState, vModEnv, vParsedFiles, vRepos
--) where

import Control.Category
import Data.Label
import Prelude hiding ((.), id)

import Control.Applicative
import Control.Monad
import Control.Monad.State as S
import Control.Monad.Error
import Control.Monad.Trans(liftIO)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Bimap as Bimap


import qualified System.FilePath as FP
import qualified System.Directory as Dir

import Lang.Common.AST
import qualified Lang.Module.AST as MA
import qualified Utils.OrdSet as OrdSet
import qualified Lang.Core.Units as U
import Utils.Utils

newtype SysExcept a = SysExceptC { runSysExceptC :: StateT SysState MExcept a }
    deriving (Monad, MonadError String, MonadState SysState, Functor, Applicative)

-- extract and run the state
runSysExcept :: SysExcept a -> SysState -> MExcept (a, SysState)
runSysExcept m st = runStateT (runSysExceptC m) st

newtype SysExceptIO a = SysExceptIOC { runSysExceptIOC :: StateT SysState MExceptIO a }
    deriving (Monad, MonadError String, MonadState SysState, MonadIO, Functor, Applicative)

-- extract and run the state
runSysExceptIO :: SysExceptIO a -> SysState -> MExceptIO (a, SysState)
runSysExceptIO m st = runStateT (runSysExceptIOC m) st


-- convert from SysExcept -> SysExceptIO
mkSysExceptIO :: SysExcept a  -> SysExceptIO a
mkSysExceptIO m = do
    st <- S.get
    case (runSysExcept m st) of
        Left err -> throwError err
        Right (a, st') -> put st' >> return a

data SysState = SysState
    { _debug :: Bool                -- do we enable debug mode
    , _simState :: SimState         -- simulation params
    , _modState :: ModState
    , _unitsState :: UnitsState
    } deriving Show

defSysState = SysState
    { _debug = False
    , _simState = defSimState
    , _modState = defModState
    , _unitsState = defUnitsState
    }

-- sim data, mod data, units data, other?
data SimState = SimState
    { _startTime :: Float
    , _endTime :: Float
    , _timestep :: Float            -- simulation timestep
    , _outputPeriod :: Integer      -- period with which to save simulation state to outfile, wrt timestep
    , _filename :: FilePath         -- output filename to save data to
    } deriving Show

defSimState = SimState
    { _startTime = 0
    , _endTime = 60
    , _timestep = 0.001         -- 1ms
    , _outputPeriod = 500             -- 0.5s
    , _filename = "output.bin"  -- default output file
    }

-- | Holds the ordered set of enabled repositories
type RepoSet = OrdSet.OrdSet FilePath

data ModState = ModState
    { _repos :: RepoSet                 -- list of enabled module repositories
    , _modEnv :: MA.GlobalModEnv        -- map of loaded modules
    , _parsedFiles :: Set.Set ModRoot   -- set of fully parsed files
    , _localFile :: MA.FileData         -- a speical file that holds the REPL mod data
    } deriving Show

defModState = ModState
    { _repos = OrdSet.empty           -- do we add the defaults here?
    , _modEnv = Map.empty -- map of loaded modules
    , _parsedFiles = Set.empty
    , _localFile = MA.mkFileData (mkModRoot ["<console>"])
    }


data UnitsState = UnitsState
    { _quantities :: U.QuantityBimap    -- a bimap of all quantities <-> dimension
--    , _unitAliases :: U.UnitAliasBimap  -- a bimap from unit <-> alias
    , _unitDimEnv :: U.UnitDimEnv       -- mapping from unit -> dimension
    , _convEnv :: U.ConvEnv             -- a mapping from dimension -> conv graph
    } deriving Show

defUnitsState = UnitsState
    { _quantities = Bimap.empty
--    , _unitAliases = Bimap.empty
    , _unitDimEnv = Map.empty
    , _convEnv = Map.empty
    }

-- TH splice
-- $(mkLabels [''SysState, ''SimState, ''ModState, ''UnitsState])
$(mkLabelsWith mkLabelName [''SysState, ''SimState, ''ModState, ''UnitsState])
-- post TH, all generated defs/labels are in scope

-- a few useful views from top SysState into nested labels
vModEnv :: SysState :-> MA.GlobalModEnv
vModEnv = lModEnv . lModState

vParsedFiles :: SysState :-> Set.Set ModRoot
vParsedFiles = lParsedFiles . lModState

vRepos :: SysState :-> RepoSet
vRepos = lRepos . lModState

vLocalFile :: SysState :-> MA.FileData
vLocalFile = lLocalFile . lModState

vQuantities :: SysState :-> U.QuantityBimap
vQuantities = lQuantities . lUnitsState

vUnitDimEnv :: SysState :-> U.UnitDimEnv
vUnitDimEnv = lUnitDimEnv . lUnitsState

vConvEnv :: SysState :-> U.ConvEnv
vConvEnv = lConvEnv . lUnitsState


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
