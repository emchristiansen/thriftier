module Thriftier.ModuleParent where

import Control.Lens
import System.FilePath.Posix

import Thriftier.HasValueL

{-import Thriftier.ModuleCPP-}


data ModuleParent = ModuleParent
  { _moduleparentValueL :: [String]
  } deriving (Show)
makeFields ''ModuleParent

mkParentFromFilePath :: FilePath -> ModuleParent
mkParentFromFilePath path = 
  ModuleParent $ init $ splitDirectories $ normalise path

{-mkParentFromModuleCPP :: ModuleCPP -> ModuleParent-}
{-mkParentFromModuleCPP moduleCPP = -}
  {-ModuleParent $ takeDirectory $ moduleCPP ^. valueL-}
