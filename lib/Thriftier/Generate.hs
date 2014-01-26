module Thriftier.Generate where

import Control.Lens
import Text.Printf
import Data.List
import Text.Regex
import System.FilePath.Posix

import Thriftier.HandlerStub
import Thriftier.CPPFile
import Thriftier.Util
import Thriftier.ImplementationRoot

generateHandler :: ImplementationRoot -> ModuleCPP -> IO ()
generateHandler implementationRoot skeletonModuleCPP = do
  file <- fromSkeleton implementationRoot skeletonModuleCPP
  writeFileUnlessExists
    (joinPath 
      [ implementationRoot ^. valueL
      , (mkModuleCPP (file ^. moduleL)) ^. valueL
      ])
    (renderAsCPP file)
  writeFile
    (joinPath 
      [ implementationRoot ^. valueL
      , (mkModuleHPP (file ^. moduleL)) ^. valueL
      ])
    (renderAsHPP file)
