module Thriftier.ModuleCPP where

import Control.Lens
import System.FilePath.Posix

import Thriftier.Module

data ModuleCPP = ModuleCPP
  { _modulecppValueL :: FilePath
  } deriving (Show)
makeFields ''ModuleCPP

mkModuleCPP :: Module -> ModuleCPP
mkModuleCPP module' = ModuleCPP $ addExtension (module' ^. valueL) ".cpp"
