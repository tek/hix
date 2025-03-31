module Hix.Test.Optparse where

import Hedgehog ((===), evalEither)
import Path (
  Abs,
  Path,
  SomeBase(..),
  absdir,
  absfile,
  parseSomeDir,
  parseSomeFile,
  )

import Hix.Optparse (absPathOrCwd)
import Hix.Test.Utils (UnitTest)

test_absPathOrCwd :: UnitTest
test_absPathOrCwd = do
  doTest parseSomeDir "../dir" [absdir|/root/dir|]
  doTest parseSomeDir "dir" [absdir|/root/base/dir|]
  doTest parseSomeDir "/dir" [absdir|/dir|]
  doTest parseSomeFile "../file.txt" [absfile|/root/file.txt|]
  doTest parseSomeFile "file.txt" [absfile|/root/base/file.txt|]
  doTest parseSomeFile "/file.txt" [absfile|/file.txt|]
  where
    doTest ::
      (String -> Either e (SomeBase t)) ->
      FilePath ->
      Path Abs t ->
      UnitTest
    doTest parse raw expect = do
      res <- evalEither $ absPathOrCwd "" [absdir|/root/base|] parse raw
      res === expect
