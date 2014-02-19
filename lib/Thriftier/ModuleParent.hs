module Thriftier.ModuleParent where

import Control.Lens
import System.FilePath.Posix

import Thriftier.ModuleCPP

data ModuleParent = ModuleParent
  { _moduleparentValueL :: FilePath
  } deriving (Show)
makeFields ''ModuleParent

mkParentFromModuleCPP :: ModuleCPP -> ModuleParent
mkParentFromModuleCPP moduleCPP = 
  ModuleParent $ takeDirectory $ moduleCPP ^. valueL
