module Thriftier.Util where

import Data.List
import System.Directory

type RelativePath = FilePath

mkString :: String -> String -> String -> [String] -> String
mkString start separator end list = 
  start ++
  intercalate separator list ++
  end

mkString' :: String -> [String] -> String
mkString' separator = mkString "" separator ""

writeFileUnlessExists :: FilePath -> String -> IO ()
writeFileUnlessExists path string = do
  exists <- doesFileExist path
  if not exists
    then writeFile path string
    else return ()
