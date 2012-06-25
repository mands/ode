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
-- |
--
-----------------------------------------------------------------------------

module UI.SysState (
SysState(..), mkDefSysState,
RepoSet
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans(liftIO)

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified System.FilePath as FP
import qualified System.Directory as Dir

import Lang.Common.AST
import qualified Lang.Module.AST as MA
import qualified Utils.OrdSet as OrdSet

-- TODO - create monad around state, use lenses

-- | Main system state used by the shell
data SysState = SysState    { stDebug :: Bool               -- do we enable debug mode
                            , stSimStart :: Float           -- simulation params
                            , stSimEnd :: Float
                            , stSimTimestep :: Float        -- simulation timestep
                            , stOutPeriod     :: Integer    -- period with which to save simulation state to outfile, wrt timestep
                            , stOutFilename :: FilePath     -- output filename to save data to
                            , stRepos :: RepoSet       -- list of enabled module repositories
                            , stGlobalModEnv :: MA.GlobalModEnv   -- map of loaded modules
                            , stParsedFiles :: Set.Set ModRoot  -- a set of fully parsed files
                            , stLocalFile :: MA.FileData        -- a speical file that holds the REPL mod data
                            -- what else??
                            } deriving Show

-- | Sensible default values for initial system state
defSysState = SysState   { stDebug = False
                        , stSimStart = 0
                        , stSimEnd = 60
                        , stSimTimestep = 0.001         -- 1ms
                        , stOutPeriod = 500             -- 0.5s
                        , stOutFilename = "output.bin"  -- default output file
                        , stRepos = OrdSet.empty           -- do we add the defaults here?
                        , stGlobalModEnv = Map.empty -- map of loaded modules
                        , stParsedFiles = Set.empty
                        , stLocalFile = MA.mkFileData (mkModRoot ["<console>"])
                        }

mkDefSysState :: IO SysState
mkDefSysState = do
    repos <- defRepos
    return $ defSysState { stRepos = repos }


-- | Holds the ordered set of enabled repositories
type RepoSet = OrdSet.OrdSet FilePath

-- need to take out of IOdef
defRepos :: IO RepoSet
defRepos = repos
  where
    repos = liftM OrdSet.fromList $ sequence [curDir, userDir]
    curDir = Dir.getCurrentDirectory
    userDir = FP.combine <$> Dir.getAppUserDataDirectory "ode" <*> pure "repos"
    sysDir = undefined
