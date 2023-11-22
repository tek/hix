-- |Combinators for @optparse-applicative@.
module Hix.Optparse where

import Data.Aeson (Value, eitherDecodeFileStrict', eitherDecodeStrict')
import Exon (exon)
import Options.Applicative (ReadM, readerError)
import Options.Applicative.Types (readerAsk)
import Path (Abs, Dir, File, Path, Rel, parseAbsDir, parseAbsFile, parseRelDir, parseRelFile, toFilePath)
import qualified Text.Show as Show

import Hix.Data.OutputFormat (OutputFormat (..))
import Hix.Managed.Handlers.Bump (SpecialBumpHandlers (TestBumpHandlers))
import Hix.Managed.Handlers.LowerInit (SpecialLowerInitHandlers (TestLowerInitHandlers))
import Hix.Managed.Handlers.LowerOptimize (SpecialLowerOptimizeHandlers (TestLowerOptimizeHandlers))

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
absDirOption :: ReadM (Path Abs Dir)
absDirOption = do
  raw <- readerAsk
  leftA (const (readerError [exon|not a valid absolute dir path: #{raw}|])) (parseAbsDir raw)

-- |A relative dir path option for @optparse-applicative@.
relDirOption :: ReadM (Path Rel Dir)
relDirOption = do
  raw <- readerAsk
  leftA (const (readerError [exon|not a valid relative dir path: #{raw}|])) (parseRelDir raw)

newtype JsonConfig =
  JsonConfig { unJsonConfig :: IO (Either String Value) }
  deriving stock (Generic)

instance Show JsonConfig where
  show (JsonConfig _) = "JsonConfig"

jsonOption ::
  ReadM JsonConfig
jsonOption = do
  raw <- readerAsk
  pure $ JsonConfig $ case parseAbsFile raw of
    Just f -> eitherDecodeFileStrict' (toFilePath f)
    Nothing -> pure (eitherDecodeStrict' (encodeUtf8 raw))

bumpHandlersOption :: ReadM SpecialBumpHandlers
bumpHandlersOption =
  readerAsk >>= \case
    "test" -> pure TestBumpHandlers
    h -> fail [exon|Invalid value for bump handlers: #{h}|]

lowerInitHandlersOption :: ReadM SpecialLowerInitHandlers
lowerInitHandlersOption =
  readerAsk >>= \case
    "test" -> pure TestLowerInitHandlers
    h -> fail [exon|Invalid value for lower.init handlers: #{h}|]

lowerOptimizeHandlersOption :: ReadM SpecialLowerOptimizeHandlers
lowerOptimizeHandlersOption =
  readerAsk >>= \case
    "test" -> pure TestLowerOptimizeHandlers
    h -> fail [exon|Invalid value for lower.optimize handlers: #{h}|]

outputFormatOption :: ReadM OutputFormat
outputFormatOption =
  readerAsk >>= \case
    "none" -> pure OutputNone
    "json" -> pure OutputJson
    fmt -> fail [exon|Invalid output format: #{fmt}|]
