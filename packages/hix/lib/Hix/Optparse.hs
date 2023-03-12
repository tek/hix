-- |Combinators for @optparse-applicative@.
module Hix.Optparse where

import Exon (exon)
import Options.Applicative (ReadM, readerError)
import Options.Applicative.Types (readerAsk)
import Path (Abs, File, Path, Rel, parseAbsFile, parseRelFile)

-- |An absolute file path option for @optparse-applicative@.
absFileOption :: ReadM (Path Abs File)
absFileOption = do
  raw <- readerAsk
  leftA (const (readerError [exon|not a valid absolute file path: #{raw}|])) (parseAbsFile raw)

-- |An relative file path option for @optparse-applicative@.
relFileOption :: ReadM (Path Rel File)
relFileOption = do
  raw <- readerAsk
  leftA (const (readerError [exon|not a valid absolute file path: #{raw}|])) (parseRelFile raw)
