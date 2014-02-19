module Thriftier.InterfaceRoot where

import Control.Lens

data InterfaceRoot = InterfaceRoot
  { _interfacerootValueL :: FilePath
  } deriving (Show)
makeFields ''InterfaceRoot
