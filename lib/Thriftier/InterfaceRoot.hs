module Thriftier.InterfaceRoot where

import Control.Lens
{-import Control.Lens.TH-}

import Thriftier.HasValueL

data InterfaceRoot = InterfaceRoot
  { _interfacerootValueL :: FilePath
  } deriving (Show)
makeFields ''InterfaceRoot
