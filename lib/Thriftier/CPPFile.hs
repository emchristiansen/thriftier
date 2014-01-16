module Thriftier.CPPFile where

import Control.Lens
import Text.Printf
import Data.List
import Text.Regex
import System.FilePath.Posix

import Thriftier.HandlerStub
import Thriftier.Util

data CPPFile = CPPFile
  { _cppfilePathL :: [String]
  , _cppfileIncludesL :: [String]
  , _cppfileDefinitionL :: String
  }
makeFields ''CPPFile

cppRelativePath :: CPPFile -> FilePath
cppRelativePath file = addExtension (joinPath $ file ^. pathL) ".cpp"

hppRelativePath :: CPPFile -> FilePath
hppRelativePath file = addExtension (joinPath $ file ^. pathL) ".hpp"

mkCPPFile :: HandlerStub -> [String] -> CPPFile
mkCPPFile stub directoryName = CPPFile 
  (directoryName ++ [handlerName stub])
  (stub ^. includesL) 
  (stub ^. bodyL)

fromSkeleton :: FilePath -> IO CPPFile
fromSkeleton skeletonPath = do
  skeletonCode <- readFile skeletonPath
  putStrLn skeletonCode
  putStrLn skeletonPath
  putStrLn $ takeDirectory skeletonPath
  putStrLn $ takeDirectory $ normalise skeletonPath
  return $ mkCPPFile
    (mkHandlerStub skeletonCode)
    (splitDirectories $ normalise $ takeDirectory skeletonPath)

renderAsCPP :: CPPFile -> String
renderAsCPP file = unlines $ 
  [printf "#include \"%s\"" $ hppRelativePath file] ++ 
  [""] ++ 
  file ^. includesL ++
  [""] ++ 
  [file ^. definitionL]

renderAsHPP :: CPPFile -> String
renderAsHPP file = 
  let 
    guard = mkString "_" "_" "_" $ file ^. pathL 
    declaration = (init $ takeWhile (/= '{') $ file ^. definitionL) ++ ";"
  in unlines $
    [printf "#ifndef %s" guard] ++
    [printf "#define %s" guard] ++
    [""] ++
    file ^. includesL ++
    [""] ++
    [declaration] ++
    [""] ++
    [printf "#endif // %s" guard] 
