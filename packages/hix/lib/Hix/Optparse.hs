-- |Combinators for @optparse-applicative@.
module Hix.Optparse where

import Data.Aeson (FromJSON, eitherDecodeStrict')
import Exon (exon)
import Options.Applicative (ReadM, readerError)
import Options.Applicative.Types (readerAsk)
import Path (Abs, File, Path, Rel, parseAbsFile, parseRelFile, Dir, parseRelDir)

-- |An absolute file path option for @optparse-applicative@.
absFileOption :: ReadM (Path Abs File)
absFileOption = do
  raw <- readerAsk
  leftA (const (readerError [exon|not a valid absolute file path: #{raw}|])) (parseAbsFile raw)

-- |A relative file path option for @optparse-applicative@.
relFileOption :: ReadM (Path Rel File)
relFileOption = do
  raw <- readerAsk
  leftA (const (readerError [exon|not a valid relative file path: #{raw}|])) (parseRelFile raw)

-- |A relative dir path option for @optparse-applicative@.
relDirOption :: ReadM (Path Rel Dir)
relDirOption = do
  raw <- readerAsk
  leftA (const (readerError [exon|not a valid relative dir path: #{raw}|])) (parseRelDir raw)

jsonOption ::
  FromJSON a =>
  ReadM a
jsonOption = do
  raw <- readerAsk
  leftA (\ e -> readerError [exon|Invalid json: #{e}
#{raw}|]) (eitherDecodeStrict' (encodeUtf8 raw))
