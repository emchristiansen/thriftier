{-|
This should have been generated automatically with 'makeFields' from 'lens', but it doesn't seem to quite work.
So, we're doing it manually.
-}
module Thriftier.HasValueL where

import Control.Lens

class HasValueL s a | s -> a where
  valueL :: Lens' s a
