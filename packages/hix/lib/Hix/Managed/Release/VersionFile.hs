module Hix.Managed.Release.VersionFile where

import Control.Lens (IndexedTraversal', ix, (.~))
import Control.Lens.Regex.Text (Match, groups)
import qualified Data.Text.IO as Text
import Exon (exon)
import Path (Abs, File, Path, fileExtension, toFilePath)
import Path.IO (doesFileExist)

import Hix.Class.Map (nFor_, nZip)
import Hix.Data.Monad (M)
import Hix.Data.PathSpec (PathSpec)
import Hix.Data.Version (Version)
import Hix.Error (pathText)
import Hix.Managed.Data.Packages (Packages (..))
import Hix.Managed.Data.StateVersionsContext (StateVersionsContext (..), StateVersionsPackage (..))
import Hix.Managed.Release.Data.Staged (SelectedTargetView (..))
import Hix.Monad (appContextVerboseIO, clientError)
import Hix.Path (resolvePathSpec)
import Hix.Pretty (showP)
import Hix.Regex (regexMulti)

type Regex = IndexedTraversal' Int Text Match

-- | Regex to match the version field in a Cabal file.
-- Matches @version:@ followed by optional spaces and an arbitrary string to match non-standard suffixes.
cabalVersionRegex :: Regex
cabalVersionRegex = [regexMulti|^(?:version:\s*)(\S+)|]

-- | Format a version for a Nix file (quoted string).
formatNixVersion :: Version -> Text
formatNixVersion version =
  [exon|"#{showP version}"
|]

-- | Update the version in a Cabal file.
updateCabalVersion :: Version -> Text -> Text
updateCabalVersion version =
  cabalVersionRegex . groups . ix 0 .~ showP version

-- | Write a version to a version file, detecting the format from the extension.
writeVersionFile ::
  PathSpec File ->
  Version ->
  M ()
writeVersionFile pathSpec version = do
  path <- resolvePathSpec pathSpec
  case fileExtension path of
    Just ".nix" ->
      writeNixVersionFile path version
    Just ".cabal" -> do
      fileExists <- doesFileExist path
      if fileExists
      then updateCabalVersionFile path version
      else clientError [exon|Cabal version file does not exist: #{pathText path}|]
    _ ->
      clientError [exon|Unsupported version file format: #{pathText path}. Supported formats: .nix, .cabal|]

-- | Write a version to a @.nix@ file.
-- Replaces entire content with quoted version string.
writeNixVersionFile ::
  Path Abs File ->
  Version ->
  M ()
writeNixVersionFile path version =
  appContextVerboseIO [exon|writing Nix version file #{toText (toFilePath path)}|] do
    Text.writeFile (toFilePath path) (formatNixVersion version)

-- | Update the version field in an existing @.cabal@ file.
updateCabalVersionFile ::
  Path Abs File ->
  Version ->
  M ()
updateCabalVersionFile path version =
  appContextVerboseIO [exon|updating Cabal version file '#{toText (toFilePath path)}'|] do
    content <- Text.readFile (toFilePath path)
    Text.writeFile (toFilePath path) (updateCabalVersion version content)

-- | Write versions to all configured version files.
-- Uses the shared version for the global version file (if configured).
-- Per-package version files are updated with each package's release version.
writeVersionFiles ::
  Maybe Version ->
  StateVersionsContext ->
  Packages SelectedTargetView ->
  M ()
writeVersionFiles sharedVersion context targets = do
  writeGlobalVersionFile
  writePackageVersionFiles
  where
    writeGlobalVersionFile =
      sequence_ (writeVersionFile <$> context.versionFile <*> sharedVersion)

    writePackageVersionFiles =
      nFor_ @(Packages _) (nZip (,) targets context.packages) \ (target, package) ->
        for_ package.versionFile \ pathSpec ->
          writeVersionFile pathSpec target.releaseVersion

