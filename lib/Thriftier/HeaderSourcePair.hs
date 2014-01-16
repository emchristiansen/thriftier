module Thriftier.HeaderSourcePair where

import Data.Lens

data HeaderSourcePair = HeaderSourcePair
  { _headersourcepairHeaderL :: CPPSyntax
  , _headersourcepairSourceL :: CPPSyntax
  }
makeFields ''HeaderSourcePair


