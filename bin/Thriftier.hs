import System.Environment
import System.FilePath.Glob
import System.FilePath.Find
{-import System.Process-}
import System.Cmd
import System.FilePath.Posix
import Text.Printf
import Text.Regex
import System.Directory
import Control.Lens
import Control.Monad

import Thriftier.Generate
import Thriftier.Language
import Thriftier.ImplementationRoot

thriftCommand :: InterfaceRoot -> ModuleThrift -> FilePath -> String
thriftCommand interfaceRoot moduleThrift implementationDirectory =
  printf 
    "cd %s; thrift -nowarn -I . --gen cpp:include_prefix -out %s %s" 
    (normalise $ interfaceRoot ^. valueL)
    (normalise implementationDirectory)
    (normalise $ moduleThrift ^. valueL)

runThrift :: InterfaceRoot -> ImplementationRoot -> ModuleThrift -> IO ()
runThrift interfaceRoot implementationRoot moduleThrift = do
  let 
    implementationDirectory = takeDirectory $ joinPath 
      [ implementationRoot ^. valueL
      , moduleThrift ^. valueL
      ]
  createDirectoryIfMissing True implementationDirectory 
  let 
    command = thriftCommand interfaceRoot moduleThrift implementationDirectory
  putStrLn command
  system command
  return ()

main :: IO ()
main = do
  [interfaceRootValue, implementationRootValue] <- getArgs
  let 
    interfaceRoot = InterfaceRoot interfaceRootValue
    implementationRoot = ImplementationRoot implementationRootValue
  {-thriftierPaths <- find always (fileName ~~? "*.thriftier") interfaceRoot-}
  {-putStrLn $ show thriftierPaths-}
  {-interfaceDirectories <- -}
    {-find always (fileType ==? Directory) (interfaceRoot ^. valueL)-}
  {-putStrLn $ show interfaceDirectories-}
  {-let -}
    {-makeOutputDirectory interfaceDirectory = do-}
      {-let outputDirectory = joinPath [outputRoot, interfaceDirectory]-}
      {-createDirectoryIfMissing True outputDirectory-}
  {-mapM makeOutputDirectory interfaceDirectories-}
  {-mapM_ (writeThriftierToThrift interfaceRoot) thriftierPaths-}
  thriftModules <- liftM (map (ModuleThrift . (makeRelative (interfaceRoot ^. valueL)))) $
    find always (fileName ~~? "*.thrift") (interfaceRoot ^. valueL)
  putStrLn $ show thriftModules
  mapM_ (runThrift interfaceRoot implementationRoot) thriftModules
  skeletonModuleCPPs <- liftM (map (ModuleCPP . (makeRelative (implementationRoot ^. valueL)))) $ 
    find always (fileName ~~? "*_server.skeleton.cpp") (implementationRoot ^. valueL)
  putStrLn $ show skeletonModuleCPPs
  mapM_ (generateHandler implementationRoot) skeletonModuleCPPs
  -- Remove the skeleton files.
  {-mapM_ -}
    {-(\skeletonModuleCPP -> removeFile $ joinPath -}
      {-[ implementationRoot ^. valueL-}
      {-, skeletonModuleCPP ^. valueL-}
      {-]) -}
    {-skeletonModuleCPPs-}
  putStrLn "Done"


{-thrift -I . --gen cpp:include_prefix -out modules/features2d modules/features2d/features2d.thrift-}
