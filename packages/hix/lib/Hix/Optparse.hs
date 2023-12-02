-- |Combinators for @optparse-applicative@.
module Hix.Optparse where

import Data.Aeson (Value, eitherDecodeFileStrict', eitherDecodeStrict')
import Exon (exon)
import Options.Applicative (ReadM, readerError)
import Options.Applicative.Types (readerAsk)
import Path (Abs, Dir, File, Path, Rel, parseAbsDir, parseAbsFile, parseRelDir, parseRelFile, toFilePath)
import qualified Text.Show as Show

import Hix.Data.OutputFormat (OutputFormat (..))
import Hix.Data.OutputTarget (OutputTarget (..))
import Hix.Managed.Handlers.Bump (SpecialBumpHandlers (TestBumpHandlers))
import Hix.Managed.Handlers.Lower (SpecialLowerHandlers (TestLowerHandlers))

pathOption ::
  String ->
  (String -> Either e a) ->
  ReadM a
pathOption desc parse = do
  raw <- readerAsk
  leftA (const (readerError [exon|not a valid #{desc} path: #{raw}|])) (parse raw)

-- |An absolute file path option for @optparse-applicative@.
absFileOption :: ReadM (Path Abs File)
absFileOption = pathOption "absolute file" parseAbsFile

-- |A relative file path option for @optparse-applicative@.
relFileOption :: ReadM (Path Rel File)
relFileOption = pathOption "relative file" parseRelFile

-- |A relative dir path option for @optparse-applicative@.
absDirOption :: ReadM (Path Abs Dir)
absDirOption = pathOption "absolute dir" parseAbsDir

-- |A relative dir path option for @optparse-applicative@.
relDirOption :: ReadM (Path Rel Dir)
relDirOption = pathOption "relative dir" parseRelDir

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

lowerHandlersOption :: ReadM SpecialLowerHandlers
lowerHandlersOption =
  readerAsk >>= \case
    "test" -> pure TestLowerHandlers
    h -> fail [exon|Invalid value for lower handlers: #{h}|]

outputFormatOption :: ReadM OutputFormat
outputFormatOption =
  readerAsk >>= \case
    "none" -> pure OutputNone
    "json" -> pure OutputJson
    "commit-msg" -> pure OutputCommitMsg
    "ga-pr" -> pure OutputGaPr
    fmt -> fail [exon|Invalid output format: #{fmt}|]

outputTargetOption :: ReadM OutputTarget
outputTargetOption =
  readerAsk >>= \case
    "default" -> pure OutputDefault
    "stdout" -> pure OutputStdout
    other -> maybe (badFile other) (pure . OutputFile) (parseAbsFile other)
  where
    badFile f = fail [exon|Argument for --output is neither an absolute filepath nor 'default' or 'stdout': #{f}|]
