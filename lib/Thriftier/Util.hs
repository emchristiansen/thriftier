module Thriftier.Util where

import Data.List

mkString :: String -> String -> String -> [String] -> String
mkString start separator end list = 
  start ++
  intercalate separator list ++
  end

mkString' :: String -> [String] -> String
mkString' separator = mkString "" separator ""
