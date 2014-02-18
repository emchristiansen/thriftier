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

{-data Arguments = Arguments -}
  {-{ _argumentsCPPServerL :: Bool-}
  {-, _argumentsHSClientL :: Bool-}
  {-, _argumentsPYClientL :: Bool-}
  {-, _argumentsInterfaceRootL :: InterfaceRoot-}
  {-, _argumentsImplementationRootL :: ImplementationRoot-}
  {-} deriving (Show)-}
{-makeFields ''Arguments-}

data CodeType = ImplementationStub | Client deriving (Eq, Show)

implementationStubParser :: Parser CodeType
implementationStubParser = flag' ImplementationStub (long "stub")

clientStubParser :: Parser CodeType
clientStubParser = flag' Client (long "client")

codeTypeParser :: Parser CodeType
codeTypeParser = (implementationStubParser <|> clientStubParser)

data Arguments = Arguments 
  { _argumentsCodeTypeL :: CodeType
  , _argumentsLanguageL :: String
  , _argumentsInterfaceRootL :: InterfaceRoot
  , _argumentsOutputRootL :: FilePath
  } deriving (Show)
makeFields ''Arguments

argumentsParser :: Parser Arguments
argumentsParser = Arguments
  <$> codeTypeParser 
  <*> strOption 
    (long "language" <> help "Target language for the generated code.")
  <*> (InterfaceRoot <$> strOption
    (long "interface-root" <> help "Root directory of Thrift interface."))
  <*> strOption
    (long "output-root" <> help "Root directory for generated code.")

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
run (Arguments ImplementationStub _ _ _) = undefined
run (Arguments Client _ _ _) = undefined
{-run (Arguments True False False interfaceRoot implementationRoot) = -}
  {-cppServer interfaceRoot implementationRoot-}
{-run (Arguments False True False interfaceRoot implementationRoot) = -}
  {-hsClient interfaceRoot implementationRoot-}
{-run (Arguments False False True interfaceRoot implementationRoot) = -}
  {-pyClient interfaceRoot implementationRoot-}
{-run _ = putStrLn "Usage error."-}

main :: IO ()
main = execParser opts >>= run

opts :: ParserInfo Arguments 
opts = info (argumentsParser <**> helper)
  ( fullDesc
 <> progDesc "Print a greeting for TARGET"
 <> header "hello - a test for optparse-applicative" )




{-data Options = Options -}
  {-{ _options -}
    {-_optionsLanguageL :: String-}
  {-, _optionsInterfaceRootL :: InterfaceRoot-}
  {-, _optionsOutputRootL :: FilePath-}
  {-} deriving (Show)-}
{-makeFields ''Options-}



{-data Command = -}
    {-Implementation Options-}
  {-| Client Options-}


{-startParser :: Parser Command-}
{-startParser = Start <$> strOption ( long "start" )-}

{-commandParser :: Parser Command-}
{-commandParser =  -}
  {-subparser-}
    {-( command "start" (info startParser -}
      {-( progDesc "Add a file to the repository" ))-}
    {-<> command "stop" (info (pure Stop) -}
      {-( progDesc "Record changes to the repository" ))-}
  {-)-}

{-myRun :: Command -> IO ()-}
{-myRun = undefined-}

{-sample :: Parser Sample-}
{-sample = subparser-}
       {-( command "hello"-}
         {-(info hello-}
               {-(progDesc "Print greeting"))-}
      {-<> command "goodbye"-}
         {-(info (pure Goodbye)-}
               {-(progDesc "Say goodbye"))-}
       {-)-}

{-opts :: ParserInfo Command-}
{-opts = info (commandParser <**> helper) idm-}

{-main :: IO ()-}
{-main = execParser opts >>= myRun-}


