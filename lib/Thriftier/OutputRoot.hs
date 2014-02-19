module Thriftier.OutputRoot where

import Control.Lens
import System.FilePath.Posix

data OutputRoot = OutputRoot
  { _implementationrootValueL :: FilePath
  } deriving (Show)
makeFields ''OutputRoot


