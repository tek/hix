module Hix.Test.Hedgehog where

import qualified Data.Text as Text
import Hedgehog ((===))

import Hix.Test.Utils (UnitTest)

eqLines ::
  HasCallStack =>
  Text ->
  Text ->
  UnitTest
eqLines l r =
  withFrozenCallStack do
    Text.lines l === Text.lines r
