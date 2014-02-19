module Thriftier.OutputRoot where

import Control.Lens
import Text.Printf
import Data.List
import Text.Regex
import System.FilePath.Posix
import Data.String.Utils
import Data.Maybe
import Control.Lens

data OutputRoot = OutputRoot
  { _implementationrootValueL :: FilePath
  } deriving (Show)
makeFields ''OutputRoot

data InterfaceRoot = InterfaceRoot
  { _interfacerootValueL :: FilePath
  } deriving (Show)
makeFields ''InterfaceRoot

data ModuleParent = ModuleParent
  { _moduleparentValueL :: FilePath
  } deriving (Show)
makeFields ''ModuleParent

data Module = Module
  { _moduleValueL :: FilePath
  } deriving (Show)
makeFields ''Module

mkModule :: ModuleParent -> String -> Module
mkModule moduleParent leafName =
  Module $ joinPath [moduleParent ^. valueL, leafName]
  
data ModuleCPP = ModuleCPP
  { _modulecppValueL :: FilePath
  } deriving (Show)
makeFields ''ModuleCPP

mkModuleCPP :: Module -> ModuleCPP
mkModuleCPP module' = ModuleCPP $ addExtension (module' ^. valueL) ".cpp"

mkParentFromModuleCPP :: ModuleCPP -> ModuleParent
mkParentFromModuleCPP moduleCPP = 
  ModuleParent $ takeDirectory $ moduleCPP ^. valueL

data ModuleHPP = ModuleHPP
  { _modulehppValueL :: FilePath
  } deriving (Show)
makeFields ''ModuleHPP

mkModuleHPP :: Module -> ModuleHPP
mkModuleHPP module' = ModuleHPP $ addExtension (module' ^. valueL) ".hpp"

data ModuleThrift = ModuleThrift
  { _modulethriftValueL :: FilePath
  } deriving (Show)
makeFields ''ModuleThrift

