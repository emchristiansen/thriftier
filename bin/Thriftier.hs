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
      skeletonPath
  in
    className

generateHandler :: FilePath -> IO ()
generateHandler skeletonPath = do
  undefined

main :: IO ()
main = do
  [interfaceRoot] <- getArgs
  thriftPaths <- find always (fileName ~~? "*.thrift") interfaceRoot
  putStrLn $ show thriftPaths
  mapM_ (runThrift interfaceRoot) thriftPaths
  putStrLn "Hi"


{-thrift -I . --gen cpp:include_prefix -out modules/features2d modules/features2d/features2d.thrift-}
