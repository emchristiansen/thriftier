module Correctness.Thriftier.HandlerStub where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.Tasty.Golden

import Data.List
import Data.Ord
import Control.Lens
import Thriftier.OutputRoot
import System.FilePath.Posix

import Thriftier.HandlerStub
import Thriftier.HasValueL
import Correctness.Thriftier.Util

hUnitTests :: TestTree
hUnitTests = testGroup "Unit tests"
  [ testCase "quoteIncludes" $ do
      skeletonCode <- readFile $ joinPath 
        [ outputRoot ^. valueL
        , joinPath $ skeletonModuleCPP ^. valueL
        ]
      (quoteIncludes skeletonCode) @?= ["#include \"OpenCV/Core/MatUtil.h\""]
    
  , testCase "handlerName" $ do
      skeletonCode <- readFile $ joinPath 
        [ outputRoot ^. valueL
        , joinPath $ skeletonModuleCPP ^. valueL
        ]
      let handler = mkHandlerStub skeletonCode
      (handlerName handler) @?= "MatUtilHandler"
  ]

goldenTests :: TestTree
goldenTests = testGroup "Golden tests"
  [ goldenVsFile' 
      "classDefinitions" $
      \goldenOut -> do
        skeletonCode <- readFile $ joinPath 
          [ outputRoot ^. valueL
          , joinPath $ skeletonModuleCPP ^. valueL
          ]
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
