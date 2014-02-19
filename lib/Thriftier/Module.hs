module Thriftier.Module where

import Control.Lens
import Control.Lens.TH
import System.FilePath.Posix

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
  { _moduletestValueL :: [String]
  } deriving (Show)
makeFields ''Module

mkModule :: ModuleParent -> String -> Module
mkModule moduleParent leafName =
  Module $ moduleParent ^. valueL ++ [leafName]

cppPath :: Module -> RelativePath
cppPath (Module value) = addExtension (joinPath value) ".cpp"

hppPath :: Module -> RelativePath
hppPath (Module value) = addExtension (joinPath value) ".hpp"

{-foo :: Module -> [String]-}
{-foo m = m ^. valueL-}

{-foo2 :: ModuleParent -> [String]-}
{-foo2 m = m ^. valueL-}

