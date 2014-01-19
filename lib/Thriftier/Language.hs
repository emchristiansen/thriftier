module Thriftier.Language where

import Control.Lens
import Text.Printf
import Data.List
import Text.Regex
import System.FilePath.Posix
import Data.String.Utils
import Data.Maybe
import Text.RawString.QQ
import Data.List.Split
import Data.Maybe
{-import qualified Data.Text as Text -}

thriftIncludes :: String -> [FilePath]
thriftIncludes thriftFile = 
  let
    getInclude = matchRegex $ mkRegex "^include \"(.+)\"$"
  in
    concat $ catMaybes $ map getInclude $ lines thriftFile

thriftIncludesRecursive :: FilePath -> FilePath -> IO [FilePath]
thriftIncludesRecursive root thriftPath = do
  file <- readFile $ root ++ thriftPath 
  let includes = thriftIncludes file
  transitiveIncludes <- mapM (thriftIncludesRecursive root) includes
  return $ includes ++ (concat transitiveIncludes)

multipleInheritance :: String -> [String]
multipleInheritance thriftFile =
  let
    {-getService = matchRegex $ mkRegex "^service \w+ extends \\(.+\\)"-}
    getService = matchRegex $ mkRegex [r|(service \w+ extends \(.+\))|]
  in
    concat $ catMaybes $ map getService $ lines thriftFile

removeWhitespace :: String -> String
removeWhitespace = filter (not . ('_' ==))

serviceNameAndAncestors :: String -> (String, [String])
serviceNameAndAncestors serviceStatement =
  let 
    Just [name] = matchRegex
      (mkRegex [r|service (\w+) extends|])
      serviceStatement
    Just ancestors = matchRegex
      (mkRegex [r|service \w+ extends \((.+)\)|])
      serviceStatement
  in
    (name, map (replace " " "")  $ splitOn "," (head ancestors))

serviceContents :: String -> String -> Maybe String
serviceContents thriftFile serviceName =
  if isInfixOf (printf "service %s" serviceName) thriftFile
  then
    let 
      strippedFront = dropWhile
        (not . isInfixOf (printf "service %s" serviceName))
        (lines thriftFile)
      middle = takeWhile
        (not . isInfixOf "}")
        (tail strippedFront)
    in
      Just $ unlines middle 
  else
    Nothing

generateMultipleInheritanceService :: FilePath -> FilePath -> IO String 
generateMultipleInheritanceService root thriftPath = do
  file <- readFile $ root ++ thriftPath
  let (serviceName, ancestors) = serviceNameAndAncestors file
  includes <- thriftIncludesRecursive root thriftPath
  files <- mapM (readFile . (root ++)) includes
  let 
    serviceStatements = do
      ancestor <- ancestors
      includedFile <- files
      return $ serviceContents includedFile ancestor
    asBlock = concat $ nub $ catMaybes serviceStatements 
  return $ unlines $
    [printf "service %s {" serviceName] ++
    (intersperse "" $ lines asBlock) ++
    ["}"]

thriftierToThrift :: FilePath -> FilePath -> IO String
thriftierToThrift root thriftierPath = do
  file <- readFile $ root ++ thriftierPath
  let includes = unlines $ filter (isInfixOf "include") $ lines file
  service <- generateMultipleInheritanceService root thriftierPath
  return $ includes ++ "\n" ++ service

writeThriftierToThrift :: FilePath -> FilePath -> IO ()
writeThriftierToThrift root thriftierPath = do
  thrift <- thriftierToThrift root thriftierPath
  writeFile
    (replaceExtension (root ++ thriftierPath) "thrift")
    thrift
