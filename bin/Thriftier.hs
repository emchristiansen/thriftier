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
import Options.Applicative

data Arguments = Arguments 
  { _argumentsCPPServerL :: Bool
  , _argumentsHSClientL :: Bool
  , _argumentsPYClientL :: Bool
  , _argumentsInterfaceRootL :: InterfaceRoot
  , _argumentsImplementationRootL :: ImplementationRoot
  } deriving (Show)
makeFields ''Arguments

argumentsParser :: Parser Arguments
argumentsParser = Arguments
  <$> switch
    (long "cpp-server" <> help "Generate server-side C++ code.")
  <*> switch
    (long "hs-client" <> help "Generate client-side Haskell code.")
  <*> switch
    (long "py-client" <> help "Generate client-side Python code.")
  <*> (InterfaceRoot <$> strOption
    (long "interface-root" <> help "Root directory of Thrift interface."))
  <*> (ImplementationRoot <$> strOption
    (long "implementation-root" <> help "Root directory for generated code."))

thriftCommand :: InterfaceRoot -> ModuleThrift -> FilePath -> String -> String
thriftCommand interfaceRoot moduleThrift implementationDirectory language =
  printf 
    "cd %s; thrift -nowarn -I . --gen %s -out %s %s" 
    (normalise $ interfaceRoot ^. valueL)
    language
    (normalise implementationDirectory)
    (normalise $ moduleThrift ^. valueL)

runThrift :: InterfaceRoot -> ImplementationRoot -> (FilePath -> FilePath) -> String -> ModuleThrift -> IO ()
runThrift interfaceRoot implementationRoot tweakImplementationDirectory language moduleThrift = do
  let 
    implementationDirectory = tweakImplementationDirectory $ takeDirectory $ joinPath 
      [ implementationRoot ^. valueL
      , moduleThrift ^. valueL
      ]
  createDirectoryIfMissing True implementationDirectory 
  let 
    command = thriftCommand 
      interfaceRoot 
      moduleThrift 
      implementationDirectory
      language
  putStrLn command
  system command
  return ()

findFileGlob :: FilePath -> String -> IO [FilePath]
findFileGlob root globPattern = liftM (map (makeRelative root)) $ 
  find always (fileName ~~? globPattern) root

cppServer :: InterfaceRoot -> ImplementationRoot -> IO ()
cppServer interfaceRoot implementationRoot = do
  thriftModules <- liftM (map ModuleThrift) $ 
    findFileGlob (interfaceRoot ^. valueL) "*.thrift"
  putStrLn $ show thriftModules
  mapM_ (runThrift interfaceRoot implementationRoot id "cpp:include_prefix") thriftModules
  skeletonModuleCPPs <- liftM (map ModuleCPP) $
    findFileGlob (implementationRoot ^. valueL) "*_server.skeleton.cpp"
  putStrLn $ show skeletonModuleCPPs
  mapM_ (generateHandler implementationRoot) skeletonModuleCPPs
  -- Remove the skeleton files.
  mapM_ 
    (\skeletonModuleCPP -> removeFile $ joinPath 
      [ implementationRoot ^. valueL
      , skeletonModuleCPP ^. valueL
      ]) 
    skeletonModuleCPPs
  putStrLn "Generated C++ server code."

hsClient :: InterfaceRoot -> ImplementationRoot -> IO ()
hsClient interfaceRoot implementationRoot = do
  thriftModules <- liftM (map ModuleThrift) $ 
    findFileGlob (interfaceRoot ^. valueL) "*.thrift"
  putStrLn $ show thriftModules
  let tweakOutputDirectory = (++ "/gen-hs")
  mapM_ (runThrift interfaceRoot implementationRoot tweakOutputDirectory "hs") thriftModules
  putStrLn "Generated Haskell client code."

pyClient :: InterfaceRoot -> ImplementationRoot -> IO ()
pyClient interfaceRoot implementationRoot = do
  thriftModules <- liftM (map ModuleThrift) $ 
    findFileGlob (interfaceRoot ^. valueL) "*.thrift"
  putStrLn $ show thriftModules
  let tweakOutputDirectory = id --(++ "/gen-py")
  mapM_ (runThrift interfaceRoot implementationRoot tweakOutputDirectory "py:new_style") thriftModules
  putStrLn "Generated Python client code."

run :: Arguments -> IO ()
run (Arguments True False False interfaceRoot implementationRoot) = 
  cppServer interfaceRoot implementationRoot
run (Arguments False True False interfaceRoot implementationRoot) = 
  hsClient interfaceRoot implementationRoot
run (Arguments False False True interfaceRoot implementationRoot) = 
  pyClient interfaceRoot implementationRoot
run _ = putStrLn "Usage error."

main :: IO ()
main = execParser opts >>= run 
  where
    opts = info (helper <*> argumentsParser)
      ( fullDesc
     <> progDesc "Invoke Thriftier to generate server or client code."
     <> header "Thriftier: A tool on which makes Apache Thrift a bit nicer to use." )




