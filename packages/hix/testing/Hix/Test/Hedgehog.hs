module Hix.Test.Hedgehog (
  eqLines,
  assertRight,
  listEqZip,
  listEqTail,
  assertContains,
  assertNotContains,
  unwrapTestTCompat,
  TestTCompat (..),
  runTestTCompat,
  TestT,
) where

import Control.Monad.Catch (MonadCatch, MonadMask (..), MonadThrow)
import Control.Monad.Morph (MFunctor)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import Data.List.Extra (takeEnd, zipWithLongest)
import qualified Data.Text as Text
import Hedgehog (TestT, annotate, evalEither, failure, (===))
import Hedgehog.Internal.Property (Failure, Journal, TestT (..))

-- | Assert that a text contains a substring, with an informative error message.
assertContains ::
  ∀ m .
  Monad m =>
  HasCallStack =>
  -- | Substring to find
  Text ->
  -- | Text to search in
  Text ->
  TestT m ()
assertContains needle haystack =
  withFrozenCallStack do
    if needle `Text.isInfixOf` haystack
    then unit
    else do
      annotate ("Expected to find: " <> toString needle)
      annotate ("In text: " <> toString haystack)
      failure

-- | Assert that a text does not contain a substring, with an informative error message.
assertNotContains ::
  ∀ m .
  Monad m =>
  HasCallStack =>
  -- | Substring that should not be present
  Text ->
  -- | Text to search in
  Text ->
  TestT m ()
assertNotContains needle haystack =
  withFrozenCallStack do
    if needle `Text.isInfixOf` haystack
    then do
      annotate ("Expected NOT to find: " <> toString needle)
      annotate ("But found in text: " <> toString haystack)
      failure
    else unit

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
  withFrozenCallStack do
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
  withFrozenCallStack do
    target === takeEnd (length target) actual

newtype TestTCompat m a =
  TestTCompat (TestT m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadTrans, MFunctor)

runTestTCompat :: TestTCompat m a -> TestT m a
runTestTCompat (TestTCompat ma) =
  ma

unwrapTestTCompat :: TestTCompat m a -> ExceptT Failure (Lazy.WriterT Journal m) a
unwrapTestTCompat =
  coerce

instance MonadMask m => MonadMask (TestTCompat m) where
  mask f =
    TestTCompat $ TestT $ mask \ restore ->
      unTest (runTestTCompat (f (coerce . restore . coerce)))

  uninterruptibleMask f =
    TestTCompat $ TestT $ uninterruptibleMask \ restore ->
      unTest (runTestTCompat (f (coerce . restore . coerce)))

  generalBracket = coerce (generalBracket @(ExceptT Failure (Lazy.WriterT Journal m)))
