module Thriftier.Module where

import Control.Lens
import System.FilePath.Posix

import Thriftier.ModuleParent

{-|
Test
Next line.
'ModuleParent'
Hi.
-}
data Module = Module
  { _moduleValueL :: FilePath
  } deriving (Show)
makeFields ''Module

mkModule :: ModuleParent -> String -> Module
mkModule moduleParent leafName =
  Module $ joinPath [moduleParent ^. valueL, leafName]

