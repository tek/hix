module Hix.Test.Hedgehog where

import qualified Data.Text as Text
import Hedgehog (TestT, evalEither, (===))

eqLines ::
  ∀ m .
  Monad m =>
  HasCallStack =>
  Text ->
  Text ->
  TestT m ()
eqLines l r =
  withFrozenCallStack do
    Text.lines l === Text.lines r

assertRight ::
  ∀ a m e .
  Eq a =>
  Show e =>
  Show a =>
  Monad m =>
  HasCallStack =>
  a ->
  Either e a ->
  TestT m ()
assertRight a =
  withFrozenCallStack do
    (===) a <=< evalEither
