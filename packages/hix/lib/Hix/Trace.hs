module Hix.Trace where

import Distribution.Pretty (Pretty)
import GHC.Stack (callStack)
import System.IO.Unsafe (unsafePerformIO)

import Hix.Pretty (showP)

trp ::
  ∀ b a .
  Pretty b =>
  HasCallStack =>
  b ->
  a ->
  a
trp b a =
  unsafePerformIO (a <$ debugPrint (srcLoc callStack) (showP b))

trpi ::
  Pretty a =>
  HasCallStack =>
  a ->
  a
trpi a =
  unsafePerformIO (a <$ debugPrint (srcLoc callStack) (showP a))
