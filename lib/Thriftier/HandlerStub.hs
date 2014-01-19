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

quoteIncludes :: String -> [String]
quoteIncludes skeletonCode =
  let
    Just includes = matchRegex
      (mkRegex "^(#include \".*\")$")
      skeletonCode
  in
    includes

classDefinitions :: String -> [String]
classDefinitions  skeletonCode =
  let
    Just definitions = matchRegex
      (mkRegex "(class (.|\n)*};)")
      skeletonCode
  in
    definitions

mkHandlerStub :: String -> HandlerStub
mkHandlerStub skeletonCode = HandlerStub
  (quoteIncludes skeletonCode)
  (head $ classDefinitions skeletonCode)

handlerName :: HandlerStub -> String
handlerName stub = 
  let 
    Just className = matchRegex
      (mkRegex "class (.+)Handler")
      (stub ^. bodyL)
  in
    printf "%sHandler" $ head className


       
