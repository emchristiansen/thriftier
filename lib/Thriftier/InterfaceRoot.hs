module Thriftier.InterfaceRoot where

import Control.Lens
import Control.Lens.TH

data InterfaceRoot = InterfaceRoot
  { _interfacerootValueL :: FilePath
  } deriving (Show)
makeFields ''InterfaceRoot
