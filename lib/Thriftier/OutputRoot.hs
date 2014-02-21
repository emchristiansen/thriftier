module Thriftier.OutputRoot where

import Control.Lens
{-import Control.Lens.TH-}
{-import System.FilePath.Posix-}

import Thriftier.HasValueL

data OutputRoot = OutputRoot
  { _implementationrootValueL :: FilePath
  } deriving (Show)
makeFields ''OutputRoot


