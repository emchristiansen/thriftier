module Thriftier.ModuleHPP where

import Control.Lens
import System.FilePath.Posix

import Thriftier.Module

data ModuleHPP = ModuleHPP
  { _modulehppValueL :: FilePath
  } deriving (Show)
makeFields ''ModuleHPP

mkModuleHPP :: Module -> ModuleHPP
mkModuleHPP module' = ModuleHPP $ addExtension (module' ^. valueL) ".hpp"
