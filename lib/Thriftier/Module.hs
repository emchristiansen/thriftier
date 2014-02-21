module Thriftier.Module where

import Control.Lens
{-import Control.Lens.TH-}
import System.FilePath.Posix

import Thriftier.HasValueL
import Thriftier.ModuleParent
import Thriftier.Util

{-|
The full name of a module as a list of strings.
For example, ["Foo", "Bar", "Baz"] might correspond to the files
"Foo/Bar/Baz.hpp" and "Foo/Bar/Baz.cpp" in C++.

Test
Next line.
'ModuleParent'
Hi.
-}
data Module = Module
  { _moduleValueL :: [String]
  } deriving (Eq, Show)
makeFields ''Module

mkModule :: ModuleParent -> String -> Module
mkModule moduleParent leafName =
  Module $ moduleParent ^. valueL ++ [leafName]

cppPath :: Module -> RelativePath
cppPath (Module value) = addExtension (joinPath value) ".cpp"

hppPath :: Module -> RelativePath
hppPath (Module value) = addExtension (joinPath value) ".hpp"

thriftPath :: Module -> RelativePath
thriftPath (Module value) = addExtension (joinPath value) ".thrift"
