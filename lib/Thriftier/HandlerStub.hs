module Thriftier.HandlerStub where

import Control.Lens
import Text.Printf
import Data.List
import Text.Regex
import System.FilePath.Posix

data HandlerStub = HandlerStub 
  { _handlerstubIncludesL :: [String]
  , _handlerstubBodyL :: String
  }
makeFields ''HandlerStub

mkHandlerStub :: String -> HandlerStub
mkHandlerStub skeletonCode =
  -- A hacky parsing of the output of Thrift.
  let
    Just includes = matchRegex
      (mkRegex "^(#include \".*\")$")
      skeletonCode
    Just body = matchRegex
      (mkRegex "(class (.|\n)*};)")
      skeletonCode
  in
    HandlerStub includes $ head body

handlerName :: HandlerStub -> String
handlerName = undefined

       
