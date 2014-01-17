module Thriftier.CPPFile where

import Control.Lens
import Text.Printf
import Data.List
import Text.Regex
import System.FilePath.Posix
import Data.String.Utils

import Thriftier.HandlerStub
import Thriftier.Util

data CPPFile = CPPFile
  { _cppfilePathL :: [String]
  , _cppfileIncludesL :: [String]
  , _cppfileDefinitionL :: String
  }
makeFields ''CPPFile

getHandlerName :: CPPFile -> String
getHandlerName file = last $ file ^. pathL

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

toDefinitionSyntax :: String -> String -> String
toDefinitionSyntax handlerName body =
  let
    Just innerClass = matchRegex
      (mkRegex "public:\n((.|\n)+)\n};")
      body
    aligned = map (drop 2) . lines $ head innerClass
    qualifyName line = 
      case matchRegex
        (mkRegex "([a-zA-Z]+)\\(.+\\{")
        line of
        Nothing -> line
        Just [name] -> replace
          (name ++ "(")
          (handlerName ++ "::" ++ name ++ "(")
          line

  in 
    unlines $ map qualifyName aligned
    {-subRegex-}
      {-(mkRegex "([a-zA-Z]+).+\\{")-}
      {-aligned-}
      {-(handlerName ++ "::\1")-}

renderAsCPP :: CPPFile -> String
renderAsCPP file = unlines $ 
  [printf "#include \"%s\"" $ hppRelativePath file] ++ 
  [""] ++ 
  file ^. includesL ++
  [""] ++ 
  [toDefinitionSyntax (getHandlerName file) (file ^. definitionL)]

renderAsHPP :: CPPFile -> String
renderAsHPP file = 
  let 
    guard = mkString "_" "_" "_" $ file ^. pathL 
    declaration = (init $ takeWhile (/= '{') $ file ^. definitionL) ++ " {};"
  in unlines $
    [printf "#ifndef %s" guard] ++
    [printf "#define %s" guard] ++
    [""] ++
    file ^. includesL ++
    [""] ++
    [declaration] ++
    [""] ++
    [printf "#endif // %s" guard] 
