import System.Environment
{-import System.FilePath.Glob-}
import System.FilePath.Find
{-import System.Process-}
import System.Cmd
import System.FilePath.Posix
import Text.Printf
{-import Text.Regex-}
import System.Directory
import Control.Lens
import Control.Monad
import Options.Applicative

import Thriftier.Generate
{-import Thriftier.Language-}
import Thriftier.OutputRoot
import Thriftier.InterfaceRoot
import Thriftier.Module
import Thriftier.ModuleParent

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
  , _argumentsOutputRootL :: OutputRoot
  } deriving (Show)
makeFields ''Arguments

argumentsParser :: FilePath -> Parser Arguments
argumentsParser pwd = Arguments
  <$> codeTypeParser 
  <*> languageParser
  {-<*> (InterfaceRoot <$> strOption-}
    {-(long "interface-root" <> help "Root directory of Thrift interface."))-}
  <*> (InterfaceRoot . (combine pwd) <$> Options.Applicative.argument str 
    (metavar "INTERFACE_ROOT" <> help "Root directory of Thrift interface."))
  <*> (OutputRoot . (combine pwd) <$> Options.Applicative.argument str 
    (metavar "OUTPUT_ROOT" <> help "Root directory for generated code."))
  {-<*> (OutputRoot <$> strOption-}
    {-(long "output-root" <> help "Root directory for generated code."))-}

thriftCommand :: InterfaceRoot -> Module -> FilePath -> String -> String
thriftCommand interfaceRoot module' outputDirectory language =
  printf 
    "cd %s; thrift -nowarn -I . --gen %s -out %s %s" 
    (normalise $ interfaceRoot ^. valueL)
    language
    (normalise outputDirectory)
    (normalise $ thriftPath module')

runThrift :: InterfaceRoot -> OutputRoot -> (FilePath -> FilePath) -> String -> Module -> IO ()
runThrift interfaceRoot outputRoot tweakOutputDirectory language module' = do
  let 
    outputDirectory = tweakOutputDirectory $ joinPath 
      [ outputRoot ^. valueL
      , joinPath $ module' ^. valueL
      ]
  createDirectoryIfMissing True outputDirectory 
  let 
    command = thriftCommand 
      interfaceRoot 
      module' 
      outputDirectory
      language
  putStrLn command
  system command
  return ()

findFileGlob :: FilePath -> String -> IO [FilePath]
findFileGlob root globPattern = liftM (map (makeRelative root)) $ 
  find always (fileName ~~? globPattern) root

pathToModule :: FilePath -> Module
pathToModule = Module . splitPath . takeDirectory

cppServer :: InterfaceRoot -> OutputRoot -> IO ()
cppServer interfaceRoot outputRoot = do
  {-thriftModules <- liftM (map Module . splitPath . takeDirectory) $ -}
    {-findFileGlob (interfaceRoot ^. Thriftier.InterfaceRoot.valueL) "*.thrift"-}
  thriftModules <- liftM (map pathToModule) $ 
    findFileGlob (interfaceRoot ^. valueL) "*.thrift"
  putStrLn $ show thriftModules
  mapM_ (runThrift interfaceRoot outputRoot id "cpp:include_prefix") thriftModules
  skeletonPaths <- findFileGlob (outputRoot ^. valueL) "*_server.skeleton.cpp"
  {-skeletonModuleCPPs <- liftM (map Module . splitPath . takeDirectory) $-}
    {-findFileGlob (outputRoot ^. Thriftier.OutputRoot.valueL) "*_server.skeleton.cpp"-}
  putStrLn $ show skeletonPaths
  mapM_ (generateHandler outputRoot) skeletonPaths
  -- Remove the skeleton files.
  mapM_ removeFile skeletonPaths
  {-mapM_ -}
    {-(\skeletonModuleCPP -> removeFile $ joinPath -}
      {-[ outputRoot ^. valueL-}
      {-, skeletonModuleCPP ^. valueL-}
      {-]) -}
    {-skeletonModuleCPPs-}
  putStrLn "Generated C++ server code."

cppClient :: InterfaceRoot -> OutputRoot -> IO ()
cppClient interfaceRoot outputRoot = do
  thriftModules <- liftM (map pathToModule) $ 
    findFileGlob (interfaceRoot ^. valueL) "*.thrift"
  putStrLn $ show thriftModules
  let tweakOutputDirectory = id
  mapM_ (runThrift interfaceRoot outputRoot tweakOutputDirectory "cpp:include_prefix") thriftModules
  putStrLn "Generated C++ client code."

hsClient :: InterfaceRoot -> OutputRoot -> IO ()
hsClient interfaceRoot outputRoot = do
  thriftModules <- liftM (map pathToModule) $ 
    findFileGlob (interfaceRoot ^. valueL) "*.thrift"
  putStrLn $ show thriftModules
  let tweakOutputDirectory = (++ "/gen-hs")
  mapM_ (runThrift interfaceRoot outputRoot tweakOutputDirectory "hs") thriftModules
  putStrLn "Generated Haskell client code."

pyClient :: InterfaceRoot -> OutputRoot -> IO ()
pyClient interfaceRoot outputRoot = do
  thriftModules <- liftM (map pathToModule) $ 
    findFileGlob (interfaceRoot ^. valueL) "*.thrift"
  putStrLn $ show thriftModules
  let tweakOutputDirectory = id --(++ "/gen-py")
  mapM_ (runThrift interfaceRoot outputRoot tweakOutputDirectory "py:new_style") thriftModules
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

description :: String
description = unlines
  [ "Generate either the server-side stub or client library code for a given language."
  , "  Takes a directory containing a Thrift interface and dumps the generated code into the output directory."
  ]

opts :: FilePath -> ParserInfo Arguments 
opts pwd = info (argumentsParser pwd <**> helper)
  ( fullDesc
 <> progDesc description 
 <> header "Thriftier: Wraps a bit of Apache Thrift and makes it a bit nicer." )

main :: IO ()
main = do
  pwd <- getCurrentDirectory 
  execParser (opts pwd) >>= run
