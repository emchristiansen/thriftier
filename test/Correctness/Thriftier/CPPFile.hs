module Correctness.Thriftier.CPPFile where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.Tasty.Golden

import Thriftier.CPPFile
import Correctness.Thriftier.Util

{-hUnitTests :: TestTree-}
{-hUnitTests = testGroup "Unit tests"-}
  {-[ testCase "" $ do-}
      {-skeletonCode <- readFile skeletonPath      -}
      {-(quoteIncludes skeletonCode) @?= ["#include \"MatUtil.h\""]-}
    
  {-, testCase "handlerName" $ do-}
      {-skeletonCode <- readFile skeletonPath-}
      {-let handler = mkHandlerStub skeletonCode-}
      {-(handlerName handler) @?= "MatUtilHandler"-}
  {-]-}

goldenTests :: TestTree
goldenTests = testGroup "Golden tests"
  [ goldenVsFile' 
      "renderAsCPP" $
      \goldenOut -> do
        skeletonCode <- readFile skeletonPath
        file <- fromSkeleton skeletonPath
        writeFile 
          goldenOut
          (renderAsCPP file)
  ]

tests = testGroup "CPPFile" [goldenTests]
