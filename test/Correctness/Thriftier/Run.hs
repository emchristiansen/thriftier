import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import Correctness.Thriftier.HandlerStub as HandlerStub
import Correctness.Thriftier.CPPFile as CPPFile 
import Correctness.Thriftier.Language as Language 

main :: IO ()
main = defaultMain $ testGroup "Thriftier" 
  [ HandlerStub.tests
  , CPPFile.tests
  , Language.tests
  ]
