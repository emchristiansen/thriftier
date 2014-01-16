module HandlerStub where

import Control.Lens
import Text.Printf
import Data.List
import Text.Regex
import System.FilePath.Posix

data HandlerStub = HandlerStub 
  { _handlerstubIncludesL :: [String]
  , _handlerstubBodyL :: String
  }
makeFields ''HandlerStub

mkHandlerStub :: String -> HandlerStub
mkHandlerStub skeletonCode =
  -- A hacky parsing of the output of Thrift.
  let
    Just includes = matchRegex
      (mkRegex "^(#include \".*\")$")
      skeletonCode
    Just body = matchRegex
      (mkRegex "(class (.|\n)*};)")
      skeletonCode
  in
    HandlerStub includes $ head body

handlerName :: HandlerStub -> String
handlerName = undefined

class Render a where
  render :: a -> String

mkString :: String -> String -> String -> [String] -> String
mkString start separator end list = 
  start ++
  concat (intersperse separator list) ++
  end

mkString' :: String -> [String] -> String
mkString' separator list = mkString "" separator "" list

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
  return $ mkCPPFile
    (mkHandlerStub skeletonCode)
    (splitDirectories $ takeDirectory skeletonPath)

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
       
generateHandler :: FilePath -> FilePath -> IO ()
generateHandler interfaceRoot skeletonRelativePath = do
  file <- fromSkeleton $ joinPath [interfaceRoot, skeletonRelativePath]
  writeFile
    (joinPath [interfaceRoot, cppRelativePath file])
    (renderAsCPP file)
  writeFile
    (joinPath [interfaceRoot, hppRelativePath file])
    (renderAsHPP file)
