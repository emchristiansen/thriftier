module Thriftier.Generate where

import Control.Lens
import Text.Printf
import Data.List
import Text.Regex
import System.FilePath.Posix

import Thriftier.HandlerStub
import Thriftier.CPPFile
import Thriftier.Util

generateHandler :: FilePath -> FilePath -> IO ()
generateHandler interfaceRoot skeletonRelativePath = do
  file <- fromSkeleton $ joinPath [interfaceRoot, skeletonRelativePath]
  writeFileUnlessExists
    (joinPath [interfaceRoot, cppRelativePath file])
    (renderAsCPP file)
  writeFile
    (joinPath [interfaceRoot, hppRelativePath file])
    (renderAsHPP file)
