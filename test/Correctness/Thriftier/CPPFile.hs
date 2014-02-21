module Correctness.Thriftier.CPPFile where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.Tasty.Golden
import Control.Lens

import Thriftier.CPPFile
import Correctness.Thriftier.Util

hUnitTests :: TestTree
hUnitTests = testGroup "Unit tests"
  [ testCase "declarations" $ do
      file <-  fromSkeleton outputRoot skeletonPath
      (declarations file) @?= 
        [ "MatUtilHandler();"
        , "void pack( ::Mat& _return, const  ::CVType::type type, const  ::MatUnpacked& matUnpacked);"
        , "void unpack( ::MatUnpacked& _return, const  ::Mat& mat);"
        ]
  ]

goldenTests :: TestTree
goldenTests = testGroup "Golden tests"
  [ goldenVsFile' 
      "renderAsCPP" $
      \goldenOut -> do
        file <-  fromSkeleton outputRoot skeletonPath
        writeFile 
          goldenOut
          (renderAsCPP file)

  , goldenVsFile' 
      "renderAsHPP" $
      \goldenOut -> do
        file <-  fromSkeleton outputRoot skeletonPath
        writeFile 
          goldenOut
          (renderAsHPP file)
  ]

tests = testGroup "CPPFile" [hUnitTests, goldenTests]
