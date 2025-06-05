module Hix.Managed.Release.Compat where

import qualified Data.Text as Text
import Exon (exon)

import qualified Hix.Color as Color
import Hix.Data.Monad (M)
import Hix.Data.Options (ReleaseOptions (..))
import qualified Hix.Log as Log
import Hix.Managed.Data.ReleaseConfig (ReleaseConfig (..))
import Hix.Managed.Release.Data.TargetSpec (targetSpecVersion)
import Hix.Monad (clientError)

compatMessage :: ReleaseOptions -> Text
compatMessage options =
  [exon|It appears you may be trying to use the old interface.

The new interface requires explicit opt-in flags for impactful operations.
For documentation, see: #{Color.url docUrl}
Run with --help to see available options.

Minimum required arguments:
  #{Color.shellCommand exampleCommand}

Or use interactive mode to select versions:
  #{Color.shellCommand interactiveCommand}
|]
  where
    docUrl :: Text
    docUrl = "https://hix.how#release"

    exampleCommand :: Text
    exampleCommand =
      [exon|nix run .#release --#{packageArgs}#{versionArg} --publish --commit --tag --merge|]

    -- Reconstruct package args from user input
    packageArgs :: Text
    packageArgs =
      mconcat [[exon| --package #{p}|] | p <- options.oldStylePackages]

    -- Reconstruct version arg from user input
    versionArg :: Text
    versionArg =
      fold $ options.oldStyleVersion <&> \ v -> [exon| --version #{v}|]

    interactiveCommand :: Text
    interactiveCommand =
      "nix run .#release -- --interactive --publish --commit --tag --merge"

usingOldInterface :: ReleaseOptions -> Bool
usingOldInterface ReleaseOptions {oldStyleVersion, oldStylePackages, config} =
  hasOldStyleArgs || noVersionSpecified
  where
    hasOldStyleArgs =
      isJust oldStyleVersion || not (null oldStylePackages)

    noVersionSpecified =
      isNothing config.version
      && not config.forceVersion
      && not config.interactive
      && not hasExplicitTargetVersions

    -- Check if any target specs include explicit versions (e.g., --package foo-1.2.3)
    hasExplicitTargetVersions =
      case config.targets of
        Nothing -> False
        Just targets -> any (isJust . targetSpecVersion) targets

-- | Print a warning and exit with error if the user appears to be using the old command interface, indicated by:
--
-- 1. Old-style arguments (@-v@ for version, positional arguments for packages)
-- 2. Missing required version specification (@--version@, @--force-version@, or @--interactive@)
cliCompat :: ReleaseOptions -> M ()
cliCompat options =
  when (usingOldInterface options) do
    Log.hang.warn "The release command's interface has changed." (Text.lines (compatMessage options))
    clientError "No version was specified. See --help for details."
