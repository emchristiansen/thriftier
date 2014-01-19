module Correctness.Thriftier.HandlerStub where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.Tasty.Golden

import Data.List
import Data.Ord

import Thriftier.HandlerStub

skeletonPath :: FilePath
skeletonPath = "test/data/gen-cpp/MatUtil_server.skeleton.cpp"

hUnitTests :: TestTree
hUnitTests = testGroup "Unit tests"
  [ testCase "quoteIncludes" $ do
      skeletonCode <- readFile skeletonPath      
      (quoteIncludes skeletonCode) @?= ["#include \"MatUtil.h\""]

  ]

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

goldenTests :: TestTree
goldenTests = testGroup "Golden tests"
  [ goldenVsFile' 
      "classDefinitions" $
      \goldenOut -> do
        skeletonCode <- readFile skeletonPath
        writeFile 
          goldenOut
          (head $ classDefinitions skeletonCode)
  ]

{-properties :: TestTree-}
{-properties = testGroup "Properties"-}
  {-[ -}
    {-QC.testProperty "sort == sort . reverse" $-}
      {-\list -> sort (list :: [Int]) == sort (reverse list)-}
  {-, QC.testProperty "Fermat's little theorem" $-}
      {-\x -> ((x :: Integer)^7 - x) `mod` 7 == 0-}
  {--- the following property does not hold-}
  {-, QC.testProperty "Fermat's last theorem" $-}
      {-\x y z n ->-}
        {-(n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)-}
  {-]-}

{-tests = testGroup "HandlerStub" [unitTests, properties]-}
tests = testGroup "HandlerStub" [hUnitTests, goldenTests]
