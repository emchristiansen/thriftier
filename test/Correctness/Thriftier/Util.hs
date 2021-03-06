module Correctness.Thriftier.Util where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.Tasty.Golden

import Thriftier.OutputRoot
import Thriftier.InterfaceRoot
import Thriftier.Module

interfaceRoot :: InterfaceRoot
interfaceRoot = InterfaceRoot "test/data/OpenCVInterface"

outputRoot :: OutputRoot
outputRoot = OutputRoot "test/data/OpenCVImplementation"

skeletonModuleCPP :: Module 
skeletonModuleCPP = Module ["OpenCV", "Core", "MatUtilHandler"]

skeletonPath :: FilePath
skeletonPath = "OpenCV/Core/MatUtil_server.skeleton.cpp"

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
