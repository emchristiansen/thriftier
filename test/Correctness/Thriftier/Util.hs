module Correctness.Thriftier.Util where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.Tasty.Golden

skeletonPath :: FilePath
skeletonPath = "test/data/gen-cpp/MatUtil_server.skeleton.cpp"

goldenPath :: FilePath
goldenPath = "test/data/golden/"

goldenVsFile' :: TestName -> (FilePath -> IO ()) -> TestTree
goldenVsFile' name mkEstimate = 
  let
    golden = goldenPath ++ name 
    goldenOut =  golden ++ ".goldenOut"
  in
    goldenVsFile
      name
      golden
      goldenOut
      (mkEstimate goldenOut)
