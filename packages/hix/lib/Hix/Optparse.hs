-- |Combinators for @optparse-applicative@.
module Hix.Optparse where

import Data.List.Extra (stripInfix)
import Distribution.Parsec (Parsec, eitherParsec, simpleParsec)
import Exon (exon)
import Options.Applicative (ReadM, eitherReader)
import Path (File, Path, Rel, parseAbsFile, parseRelFile)

import Hix.Data.OutputFormat (OutputFormat (..))
import Hix.Data.OutputTarget (OutputTarget (..))
import Hix.Managed.Cabal.ContextHackageRepo (fieldUpdater)
import Hix.Managed.Cabal.Data.ContextHackageRepo (ContextHackageRepo)
import Hix.Managed.Cabal.Data.HackageRepo (HackageIndexState, HackageName)
import Hix.Managed.Data.BuildConfig (SpecialBuildHandlers (..))
import Hix.Managed.Data.ReleaseConfig (ArtifactConfig (..), CandidatesSpec (..), ReleaseVersion (..))
import Hix.Managed.Data.SpecialMaintHandlers (SpecialMaintHandlers (..))
import Hix.Managed.Data.VersionIncrement (VersionIncrement (..))
import qualified Hix.Ui.Data.Theme as Theme
import Hix.Ui.Data.Theme (Theme)

pathOption ::
  String ->
  (String -> Either e a) ->
  ReadM a
pathOption desc parse =
  eitherReader \ raw ->
    first (const [exon|not a valid #{desc} path: #{raw}|]) (parse raw)

-- | A relative file path option for @optparse-applicative@.
relFileOption :: ReadM (Path Rel File)
relFileOption = pathOption "relative file" parseRelFile

buildHandlersOption :: ReadM SpecialBuildHandlers
buildHandlersOption =
  eitherReader \case
    "test" -> Right BuildHandlersTestBump
    "test-maint" -> Right BuildHandlersTestMaint
    h -> Left [exon|Invalid value for build handlers: #{h}|]

maintHandlersOption :: ReadM SpecialMaintHandlers
maintHandlersOption =
  eitherReader \case
    "test-maint" -> Right MaintHandlersTestMaint
    h -> Left [exon|Invalid value for maint handlers: #{h}|]

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
    "github" -> Right OutputGithub
    other -> maybe (badFile other) (Right . OutputFile) (parseAbsFile other)
  where
    badFile f = Left [exon|Argument for --output is neither an absolute filepath nor 'default' or 'stdout': #{f}|]

themeOption :: ReadM Theme
themeOption =
  eitherReader \case
    "default" -> Right Theme.Default
    "native" -> Right Theme.Native
    other -> Left [exon|Invalid theme name: #{other}|]

parsecOption :: Parsec a => Text -> ReadM a
parsecOption desc =
  eitherReader \ raw -> first (err raw) (eitherParsec raw)
  where
    err raw msg = [exon|'#{raw}' is not a valid ##{desc}: #{msg}|]

indexStateOption :: ReadM HackageIndexState
indexStateOption = parsecOption "index state"

hackageRepoFieldOption :: ReadM (HackageName, ContextHackageRepo -> ContextHackageRepo)
hackageRepoFieldOption =
  eitherReader \ spec -> do
    (name, rest) <- takeField spec
    (field, value) <- takeField rest
    update <- fieldUpdater field value
    pure (fromString name, update)
  where
    takeField = maybeToRight "Invalid Hackage repo field specification" . stripInfix ":"

nonOption :: IsString a => ReadM a
nonOption =
  eitherReader \case
    option@('-' : _) -> Left [exon|Invalid option: #{option}|]
    a -> Right (fromString a)

deprecatedOption :: String -> ReadM a
deprecatedOption new =
  eitherReader \ _ -> Left [exon|Deprecated in favor of #{new}|]

releaseVersionOption :: ReadM ReleaseVersion
releaseVersionOption =
  eitherReader \ spec ->
    maybeToRight invalid (
      (ConcreteVersion <$> simpleParsec spec)
      <|>
      (VersionIncrement <$> increment spec)
      <|>
      (keep spec)
    )
  where
    increment = \case
      "super" -> Just Supermajor
      "s" -> Just Supermajor
      "major" -> Just Major
      "m" -> Just Major
      "minor" -> Just Minor
      "n" -> Just Minor
      "patch" -> Just Patch
      "p" -> Just Patch
      _ -> Nothing

    keep = \case
      "keep" -> Just KeepVersion
      "k" -> Just KeepVersion
      _ -> Nothing

    invalid = "Invalid release version: must be a valid version or super|major|minor|patch|keep"

publishOption :: ReadM ArtifactConfig
publishOption =
  eitherReader \case
    "all" -> conf True True
    "sources" -> conf True False
    "docs" -> conf False True
    "none" -> conf False False
    value -> Left [exon|Invalid value for --publish: '#{value}' [all|sources|docs|none]|]
  where
    conf sources docs = Right ArtifactConfig {..}

candidatesOption :: ReadM CandidatesSpec
candidatesOption =
  eitherReader \case
    "auto" -> Right CandidatesAuto
    "all" -> conf True True
    "sources" -> conf True False
    "docs" -> conf False True
    "none" -> conf False False
    value -> Left [exon|Invalid value for --candidates: '#{value}' [all|auto|sources|docs|none]|]
  where
    conf sources docs = Right CandidatesSelected {..}
