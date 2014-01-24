import System.Environment
import System.FilePath.Glob
import System.FilePath.Find
{-import System.Process-}
import System.Cmd
import System.FilePath.Posix
import Text.Printf
import Text.Regex
import System.Directory

import Thriftier.Generate
import Thriftier.Language

thriftCommand :: FilePath -> FilePath -> FilePath -> String
thriftCommand interfaceRoot thriftFile outputDirectory =
  {-let directory = takeDirectory thriftFile in-}
  printf 
    "cd %s; thrift -I %s --gen cpp:include_prefix -out %s %s" 
    interfaceRoot 
    interfaceRoot
    outputDirectory 
    (joinPath [interfaceRoot, thriftFile]) 

runThrift :: FilePath -> FilePath -> FilePath -> IO ()
runThrift interfaceRoot outputRoot thriftFile = do
  let outputDirectory = takeDirectory $ joinPath [outputRoot, thriftFile]
  createDirectoryIfMissing True outputDirectory 
  let command = thriftCommand interfaceRoot thriftFile outputDirectory
  putStrLn command
  system command
  return ()

main :: IO ()
main = do
  [interfaceRoot, outputRoot] <- getArgs
  {-thriftierPaths <- find always (fileName ~~? "*.thriftier") interfaceRoot-}
  {-putStrLn $ show thriftierPaths-}
  interfaceDirectories <- find always (fileType ==? Directory) interfaceRoot
  putStrLn $ show interfaceDirectories
  {-let -}
    {-makeOutputDirectory interfaceDirectory = do-}
      {-let outputDirectory = joinPath [outputRoot, interfaceDirectory]-}
      {-createDirectoryIfMissing True outputDirectory-}
  {-mapM makeOutputDirectory interfaceDirectories-}
  {-mapM_ (writeThriftierToThrift interfaceRoot) thriftierPaths-}
  thriftPaths <- find always (fileName ~~? "*.thrift") interfaceRoot
  putStrLn $ show thriftPaths
  mapM_ (runThrift interfaceRoot outputRoot) thriftPaths
  {-skeletonPaths <- find always (fileName ~~? "*_server.skeleton.cpp") outputRoot-}
  {-putStrLn $ show skeletonPaths-}
  {-mapM_ (generateHandler outputRoot) skeletonPaths-}
   {-Remove the skeleton files.-}
  {-mapM_ (\path -> removeFile $ joinPath [outputRoot, path]) skeletonPaths-}
  putStrLn "Done"


{-thrift -I . --gen cpp:include_prefix -out modules/features2d modules/features2d/features2d.thrift-}
