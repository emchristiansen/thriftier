import System.Environment
import System.FilePath.Glob
import System.FilePath.Find
{-import System.Process-}
import System.Cmd
import System.FilePath.Posix
import Text.Printf
import Text.Regex

thriftCommand :: FilePath -> FilePath -> String
thriftCommand interfaceRoot thriftFile =
  let directory = takeDirectory thriftFile in
  printf 
    "cd %s; thrift -I . --gen cpp:include_prefix -out %s %s" 
    interfaceRoot 
    directory 
    thriftFile  

runThrift :: FilePath -> FilePath -> IO ()
runThrift interfaceRoot thriftFile = do
  let command = thriftCommand interfaceRoot thriftFile
  putStrLn command
  system command
  return ()

skeletonPathToClassName :: FilePath -> String
skeletonPathToClassName skeletonPath =
  let 
    -- E.g. "Features2D_server.skeleton.cpp"
    fileName = takeFileName skeletonPath
    Just [className] = matchRegex
      (mkRegex "(.*)_server.skeleton.cpp")
      fileName 
  in
    className

extractHanderStub :: String -> String -> CPPFile 
extractHanderStub className skeletonCode =
  let
    Just includes = matchRegex
      (mkRegex "^(#include \".*\")$")
      skeletonCode
    Just stub = matchRegex
      (mkRegex "(class (.|\n)*};)")
      skeletonCode
  in
    -- TODO: Clean up this regex nonsense.
    CPPFile includes (head stub)

stubToHeader :: CPPFile -> CPPFile
stubToHeader (CPPFile includes body) = 
  CPPFile includes $ (init $ takeWhile (/= '{') body) ++ ";"

generateHandler :: FilePath -> IO ()
generateHandler skeletonPath = do
  let className = skeletonPathToClassName skeletonPath
  skeletonCode <- readFile skeletonPath
  let handlerPath = (takeDirectory skeletonPath) ++ "/" ++ className ++ "Handler"
  let handlerHPath = handlerPath ++ ".h"
  let (CPPFile includes body) = extractHanderStub className skeletonCode
  let handlerStub = CPPFile ("#include \"" ++ handlerHPath ++ "\"" : includes) body
  let handlerCppPath = handlerPath ++ ".cpp"
  writeFile handlerCppPath handlerStub
  let handlerHeader = stubToHeader handlerStub
  writeFile handlerHPath handlerHeader 

main :: IO ()
main = do
  [interfaceRoot] <- getArgs
  thriftPaths <- find always (fileName ~~? "*.thrift") interfaceRoot
  putStrLn $ show thriftPaths
  mapM_ (runThrift interfaceRoot) thriftPaths
  skeletonPaths <- find always (fileName ~~? "*_server.skeleton.cpp") interfaceRoot
  putStrLn $ show skeletonPaths
  mapM_ generateHandler skeletonPaths
  putStrLn "Done"


{-thrift -I . --gen cpp:include_prefix -out modules/features2d modules/features2d/features2d.thrift-}
