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
import Thriftier.Module

generateHandler :: OutputRoot -> FilePath -> IO ()
generateHandler outputRoot skeletonPath = do
  file <- fromSkeleton outputRoot skeletonPath
  writeFileUnlessExists
    (joinPath 
      [ outputRoot ^. valueL
      , cppPath (file ^. moduleL)
      ])
    (renderAsCPP file)
  writeFile
    (joinPath 
      [ outputRoot ^. valueL
      , hppPath (file ^. moduleL)
      ])
    (renderAsHPP file)
