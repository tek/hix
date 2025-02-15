-- |Combinators for @optparse-applicative@.
module Hix.Optparse where

import Data.Aeson (Value, eitherDecodeFileStrict', eitherDecodeStrict')
import Distribution.Parsec (eitherParsec)
import Exon (exon)
import Options.Applicative (ReadM, eitherReader)
import Path (
  Abs,
  Dir,
  File,
  Path,
  Rel,
  SomeBase (..),
  parseAbsDir,
  parseAbsFile,
  parseRelDir,
  parseRelFile,
  parseSomeFile,
  toFilePath,
  (</>),
  )
import qualified Text.Show as Show

import Hix.Data.OutputFormat (OutputFormat (..))
import Hix.Data.OutputTarget (OutputTarget (..))
import Hix.Managed.Cabal.Data.Config (HackageIndexState (HackageIndexState))
import Hix.Managed.Handlers.Build (SpecialBuildHandlers (TestBumpHandlers))

pathOption ::
  String ->
  (String -> Either e a) ->
  ReadM a
pathOption desc parse =
  eitherReader \ raw ->
    first (const [exon|not a valid #{desc} path: #{raw}|]) (parse raw)

absPathOrCwdOption ::
  String ->
  (String -> Either e (SomeBase t)) ->
  Path Abs Dir ->
  ReadM (Path Abs t)
absPathOrCwdOption desc parse cwd =
  eitherReader \ raw ->
    first (const [exon|not a valid #{desc} path: #{raw}|]) (parse raw) <&> \case
      Abs p -> p
      Rel p -> cwd </> p

-- | An absolute file path option for @optparse-applicative@.
absFileOption :: ReadM (Path Abs File)
absFileOption = pathOption "absolute file" parseAbsFile

-- | An absolute file path option for @optparse-applicative@.
absFileOrCwdOption :: Path Abs Dir -> ReadM (Path Abs File)
absFileOrCwdOption = absPathOrCwdOption "absolute or relative file" parseSomeFile

-- | A relative file path option for @optparse-applicative@.
relFileOption :: ReadM (Path Rel File)
relFileOption = pathOption "relative file" parseRelFile

-- | A file path option for @optparse-applicative@.
someFileOption :: ReadM (SomeBase File)
someFileOption = pathOption "some file" parseSomeFile

-- | A relative dir path option for @optparse-applicative@.
absDirOption :: ReadM (Path Abs Dir)
absDirOption = pathOption "absolute dir" parseAbsDir

-- | A relative dir path option for @optparse-applicative@.
relDirOption :: ReadM (Path Rel Dir)
relDirOption = pathOption "relative dir" parseRelDir

newtype JsonConfig =
  JsonConfig { unJsonConfig :: IO (Either String Value) }
  deriving stock (Generic)

instance Show JsonConfig where
  show (JsonConfig _) = "JsonConfig"

jsonOption :: ReadM JsonConfig
jsonOption =
  eitherReader \ raw -> do
    pure $ JsonConfig $ case parseAbsFile raw of
      Just f -> eitherDecodeFileStrict' (toFilePath f)
      Nothing -> pure (eitherDecodeStrict' (encodeUtf8 raw))

buildHandlersOption :: ReadM SpecialBuildHandlers
buildHandlersOption =
  eitherReader \case
    "test" -> Right TestBumpHandlers
    h -> Left [exon|Invalid value for build handlers: #{h}|]

outputFormatOption :: ReadM OutputFormat
outputFormatOption =
  eitherReader \case
    "none" -> Right OutputNone
    "json" -> Right OutputJson
    "commit-msg" -> Right OutputCommitMsg
    "ga-pr" -> Right OutputGaPr
    fmt -> Left [exon|Invalid output format: #{fmt}|]

outputTargetOption :: ReadM OutputTarget
outputTargetOption =
  eitherReader \case
    "default" -> Right OutputDefault
    "stdout" -> Right OutputStdout
    other -> maybe (badFile other) (Right . OutputFile) (parseAbsFile other)
  where
    badFile f = Left [exon|Argument for --output is neither an absolute filepath nor 'default' or 'stdout': #{f}|]

indexStateOption :: ReadM HackageIndexState
indexStateOption =
  eitherReader \ raw -> bimap (err raw) HackageIndexState (eitherParsec raw)
  where
    err raw msg = [exon|Invalid index state string '#{raw}': #{msg}|]
