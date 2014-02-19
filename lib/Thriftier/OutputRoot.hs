module Thriftier.OutputRoot where

import Control.Lens
{-import Control.Lens.TH-}
{-import System.FilePath.Posix-}

data OutputRoot = OutputRoot
  { _implementationrootValueL :: FilePath
  } deriving (Show)
makeFields ''OutputRoot


