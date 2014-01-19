module Correctness.Thriftier.Language where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.Tasty.Golden

import Data.List
import Data.Ord

import Thriftier.Language
import Correctness.Thriftier.Util

interfacePath :: FilePath
interfacePath = "test/data/opencvInterface/"

opencvThriftier :: FilePath
opencvThriftier = interfacePath ++ "opencv.thriftier"

hUnitTests :: TestTree
hUnitTests = testGroup "Unit tests"
  [ testCase "thriftIncludes" $ do
      code <- readFile opencvThriftier
      (thriftIncludes code) @?= 
        [ "modules/core/mat.thrift"
        , "modules/features2d/features2d.thrift"
        ]
  , testCase "multipleInheritance" $ do
      code <- readFile opencvThriftier
      (multipleInheritance code) @?= 
        [ "service OpenCV extends (MatUtil, Features2D)"
        ]
  ]

goldenTests :: TestTree
goldenTests = testGroup "Golden tests"
  [ goldenVsFile' 
      "multipleInheritance" $
      \goldenOut -> do
        thriftCode <- generateMultipleInheritanceService 
          interfacePath 
          "opencv.thriftier"
        writeFile 
          goldenOut
          thriftCode 
  , goldenVsFile' 
      "thriftierToThrift" $
      \goldenOut -> do
        thriftCode <- thriftierToThrift
          interfacePath 
          "opencv.thriftier"
        writeFile 
          goldenOut
          thriftCode 
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
tests = testGroup "Language" [hUnitTests, goldenTests]
