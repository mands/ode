-- | Special interface to allow evalImport to be called from ModDefDriver, breaking the import cycle between
-- ModCmdDriver and ModDefDriver

module Subsystem.ModDriver.ModCmd (
evalImport
) where

import Utils.Utils
import qualified Subsystem.SysState as St
import AST.Common
import AST.Module

evalImport :: LocalModEnv -> ModImport -> St.SysExceptIO LocalModEnv
