module Hix.Trace where

import GHC.Stack (callStack)
import System.IO.Unsafe (unsafePerformIO)

import Hix.Pretty (HPretty, showHP)

trp ::
  ∀ b a .
  HPretty b =>
  HasCallStack =>
  b ->
  a ->
  a
trp b a =
  unsafePerformIO (a <$ debugPrint (srcLoc callStack) (showHP b))

trpi ::
  HPretty a =>
  HasCallStack =>
  a ->
  a
trpi a =
  unsafePerformIO (a <$ debugPrint (srcLoc callStack) (showHP a))
