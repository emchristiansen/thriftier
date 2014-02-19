module Thriftier.ModuleParent where

import Control.Lens
import System.FilePath.Posix

{-import Thriftier.ModuleCPP-}

data ModuleParent = ModuleParent
  { _moduleparentValueL :: [String]
  } deriving (Show)
makeFields ''ModuleParent

mkParentFromFilePath :: FilePath -> ModuleParent
mkParentFromFilePath path = 
  ModuleParent $ init $ splitPath $ normalise path

{-mkParentFromModuleCPP :: ModuleCPP -> ModuleParent-}
{-mkParentFromModuleCPP moduleCPP = -}
  {-ModuleParent $ takeDirectory $ moduleCPP ^. valueL-}
