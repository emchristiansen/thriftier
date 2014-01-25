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
generateHandler outputRoot skeletonRelativePath = do
  file <- fromSkeleton outputRoot skeletonRelativePath
  writeFileUnlessExists
    (joinPath [outputRoot, cppRelativePath file])
    (renderAsCPP file)
  writeFile
    (joinPath [outputRoot, hppRelativePath file])
    (renderAsHPP file)
