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
