module Thriftier.Generate where

import Control.Lens
{-import Text.Printf-}
{-import Data.List-}
{-import Text.Regex-}
import System.FilePath.Posix

import Thriftier.HandlerStub
import Thriftier.CPPFile
import Thriftier.Util
import Thriftier.OutputRoot

generateHandler :: OutputRoot -> ModuleCPP -> IO ()
generateHandler outputRoot skeletonModuleCPP = do
  file <- fromSkeleton outputRoot skeletonModuleCPP
  writeFileUnlessExists
    (joinPath 
      [ outputRoot ^. valueL
      , (mkModuleCPP (file ^. moduleL)) ^. valueL
      ])
    (renderAsCPP file)
  writeFile
    (joinPath 
      [ outputRoot ^. valueL
      , (mkModuleHPP (file ^. moduleL)) ^. valueL
      ])
    (renderAsHPP file)
