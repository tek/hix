module Hix.Test.Hedgehog where

import Data.List.Extra (takeEnd, zipWithLongest)
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
    take common linesL === take common linesR
    if ll > lr
    then trailing "missing lines: " linesL
    else if lr > ll
    then trailing "extra lines: " linesR
    else unit
  where
    trailing desc ls = fail (toString (desc <> Text.unlines (drop common ls)))
    common = min ll lr
    ll = length linesL
    lr = length linesR
    linesL = Text.lines l
    linesR = Text.lines r

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

listEqZip ::
  ∀ m a .
  Monad m =>
  HasCallStack =>
  Eq a =>
  Show a =>
  [a] ->
  [a] ->
  TestT m ()
listEqZip target actual =
  for_ (zip [0 :: Natural ..] (zipWithLongest (,) target actual)) \case
    (i, (Just t, Just l)) -> (i, t) === (i, l)
    _ | [] <- actual -> fail "Result list is empty."
    _ -> target === actual

listEqTail ::
  ∀ m a .
  Monad m =>
  HasCallStack =>
  Eq a =>
  Show a =>
  [a] ->
  [a] ->
  TestT m ()
listEqTail target actual =
  target === takeEnd (length target) actual
