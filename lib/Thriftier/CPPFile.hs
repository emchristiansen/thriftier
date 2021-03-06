module Thriftier.CPPFile where

import Control.Lens
import Text.Printf
import Data.List
import Text.Regex
import System.FilePath.Posix
import Data.String.Utils
import Data.Maybe

import Thriftier.HandlerStub
import Thriftier.Util
import Thriftier.ModuleParent
import Thriftier.OutputRoot
import Thriftier.InterfaceRoot
import Thriftier.Module
import Thriftier.HasValueL

data CPPFile = CPPFile
  { _cppfileModuleL :: Module 
  , _cppfileIncludesL :: [String]
  , _cppfileDefinitionL :: String
  } deriving (Eq, Show)
makeFields ''CPPFile

getHandlerName :: CPPFile -> String
getHandlerName file = last $ file ^. moduleL ^. valueL 

{-cppRelativePath :: CPPFile -> ModuleCPP-}
{-cppRelativePath file = -}
  {-addExtension (file ^. modulePathL ^. valueL) ".cpp"-}

{-hppRelativePath :: CPPFile -> FilePath-}
{-hppRelativePath file = -}
  {-addExtension (file ^. modulePathL ^. valueL) ".hpp"-}

mkCPPFile :: HandlerStub -> ModuleParent -> CPPFile
mkCPPFile stub moduleParent = CPPFile 
  (mkModule moduleParent (handlerName stub))
  (stub ^. includesL) 
  (stub ^. bodyL)

fromSkeleton :: OutputRoot -> RelativePath -> IO CPPFile
fromSkeleton outputRoot skeletonPath = do
  skeletonCode <- readFile $ joinPath 
    [ outputRoot ^. valueL
    , skeletonPath
    ]
  {-putStrLn skeletonCode-}
  {-putStrLn skeletonPath-}
  {-putStrLn $ takeDirectory skeletonPath-}
  {-putStrLn $ takeDirectory $ normalise skeletonPath-}
  return $ mkCPPFile
    (mkHandlerStub skeletonCode)
    (mkParentFromFilePath skeletonPath)

toDefinitionSyntax :: String -> String -> String
toDefinitionSyntax handlerName body =
  let
    Just innerClass = matchRegex
      (mkRegex "public:\n((.|\n)+)\n};")
      body
    aligned = map (drop 2) . lines $ head innerClass
    qualifyName line = 
      case matchRegex
        (mkRegex "([a-zA-Z0-9]+)\\(.+\\{")
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
  [printf "#include \"%s\"" $ hppPath (file ^. moduleL)] ++ 
  [""] ++ 
  file ^. includesL ++
  [""] ++ 
  [toDefinitionSyntax (getHandlerName file) (file ^. definitionL)]

declarations :: CPPFile -> [String]
declarations file =
  let
    getDeclaration line = matchRegex
      (mkRegex "^  ([ a-zA-Z0-9]+\\(.*\\)) \\{$")
      line
    maybeMatches = map getDeclaration $ lines $ file ^. definitionL
    declarations' = concat $ catMaybes maybeMatches
  in
    map (++ ";") declarations' 

renderAsHPP :: CPPFile -> String
renderAsHPP file = 
  let 
    guard = mkString "_" "_" "_" $ file ^. moduleL ^. valueL 
    outerDeclaration = (init $ takeWhile (/= '{') $ file ^. definitionL) ++ " {"
    declaration = unlines $
      [outerDeclaration] ++
      [" public:"] ++
      (intersperse "" $ map ("  " ++) $ declarations file) ++
      ["};"]
  in unlines $
    [printf "#ifndef %s" guard] ++
    [printf "#define %s" guard] ++
    [""] ++
    file ^. includesL ++
    [""] ++
    [declaration] ++
    [""] ++
    [printf "#endif // %s" guard] 
