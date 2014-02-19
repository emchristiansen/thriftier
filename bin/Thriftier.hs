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

data CodeType = ImplementationStub | Client deriving (Eq, Show)

implementationStubParser :: Parser CodeType
implementationStubParser = flag' ImplementationStub (long "stub")

clientStubParser :: Parser CodeType
clientStubParser = flag' Client (long "client")

codeTypeParser :: Parser CodeType
codeTypeParser = implementationStubParser <|> clientStubParser

data Language = Cpp | Python | Haskell deriving (Eq, Show)

cppParser :: Parser Language
cppParser = flag' Cpp (long "cpp")

pythonParser :: Parser Language
pythonParser = flag' Python (long "python")

haskellParser :: Parser Language
haskellParser = flag' Haskell (long "haskell")

languageParser :: Parser Language
languageParser = cppParser <|> pythonParser <|> haskellParser

data Arguments = Arguments 
  { _argumentsCodeTypeL :: CodeType
  , _argumentsLanguageL :: Language
  , _argumentsInterfaceRootL :: InterfaceRoot
  , _argumentsOutputRootL :: ImplementationRoot
  } deriving (Show)
makeFields ''Arguments

argumentsParser :: Parser Arguments
argumentsParser = Arguments
  <$> codeTypeParser 
  <*> languageParser
  {-<*> (InterfaceRoot <$> strOption-}
    {-(long "interface-root" <> help "Root directory of Thrift interface."))-}
  <*> (InterfaceRoot <$> Options.Applicative.argument str 
    (metavar "INTERFACE_ROOT" <> help "Root directory of Thrift interface."))
  <*> (ImplementationRoot <$> Options.Applicative.argument str 
    (metavar "OUTPUT_ROOT" <> help "Root directory for generated code."))
  {-<*> (ImplementationRoot <$> strOption-}
    {-(long "output-root" <> help "Root directory for generated code."))-}

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

cppClient :: InterfaceRoot -> ImplementationRoot -> IO ()
cppClient interfaceRoot implementationRoot = do
  thriftModules <- liftM (map ModuleThrift) $ 
    findFileGlob (interfaceRoot ^. valueL) "*.thrift"
  putStrLn $ show thriftModules
  let tweakOutputDirectory = id
  mapM_ (runThrift interfaceRoot implementationRoot tweakOutputDirectory "cpp:include_prefix") thriftModules
  putStrLn "Generated C++ client code."

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
run (Arguments ImplementationStub Cpp interfaceRoot outputRoot) = 
  cppServer interfaceRoot outputRoot
run (Arguments Client Cpp interfaceRoot outputRoot) = 
  cppClient interfaceRoot outputRoot
run (Arguments Client Python interfaceRoot outputRoot) = 
  pyClient interfaceRoot outputRoot
run (Arguments Client Haskell interfaceRoot outputRoot) = 
  hsClient interfaceRoot outputRoot
run _ = putStrLn "Not implemented yet."

main :: IO ()
main = execParser opts >>= run

description :: String
description = 
  "Generate either the server-side stub or client library code for a given language.\
  \\n  Takes a directory containing a Thrift interface and dumps the generated code into the output directory."

opts :: ParserInfo Arguments 
opts = info (argumentsParser <**> helper)
  ( fullDesc
 <> progDesc description 
 <> header "Thriftier: Wraps a bit of Apache Thrift and makes it a bit nicer." )
