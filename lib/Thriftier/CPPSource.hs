module Thriftier.CPPSource where

import Control.Lens 

data CPPSource = CPPSource {
  _cppsourceNameL :: String,
  _cppsyntaxIncludesL :: [String],
  _cppsyntaxBodyL :: String
}
makeFields ''CPPSource

renderString :: CPPSource -> String
renderString cppSource = 
  unlines $ cppSource ^. includesL ++ [""] ++ [cppSource ^. bodyL]

extractDeclarations :: String -> String
extractDeclarations body = 
  -- A hack, but whatever.
  -- Will fix if necessary.
  (init $ takeWhile (/= '{') body) ++ ";"

writeWithHeader :: CPPSource -> FilePath -> IO ()
writeWithHeader source outPath = do
  let
    headerPath :: FilePath
    headerPath = outPath ++ ".hpp"
    header :: CPPSource
    header = source & bodyL %~ extractDeclarations
  writeFile headerPath $ renderString header
  let 
    headerInclude = "#include \"" ++ headerPath ++ "\""
    -- Add the header to the include for the source file.
    sourceWithInclude :: CPPSource
    sourceWithInclude = source & includesL %~ (headerInclude :)
    sourcePath = outPath ++ ".cpp"
  writeFile sourcePath $ renderString sourceWithInclude


