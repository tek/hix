{-# language CPP #-}

module Hix.Managed.Cabal.Upload where

import Control.Lens (IndexedTraversal', ix, (%~), (^.))
import Control.Lens.Regex.Text (Match, groups, regex)
import Data.List.Extra (unescapeHTML)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import Distribution.Client.Setup (IsCandidate (IsPublished), RepoContext)
import qualified Distribution.Client.Types.Credentials as Cabal
import qualified Distribution.Client.Upload as Upload
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import Distribution.Types.Lens (customFieldsPD, packageDescription)
import Distribution.Verbosity (Verbosity)
import Exon (exon)
import Path (Abs, File, Path, filename, toFilePath)

import Hix.Cabal (catchExitCodeM)
import qualified Hix.Color as Color
import Hix.Data.Monad (M)
import Hix.Data.PackageId (PackageId (..))
import Hix.Error (pathText)
import Hix.Hackage (fatalHackageRequest, hackageGet, hackagePostForm)
import qualified Hix.Log as Log
import Hix.Managed.Cabal.Data.Revision (Revision (..))
import Hix.Managed.Cabal.Init (SolveFlags (..))
import Hix.Managed.Cabal.PackageDescription (parseCabalFile)
import Hix.Managed.Cabal.Repo (withRepoContextM)
import Hix.Managed.Handlers.HackageClient (HackageClient (..), HackageError (..), HackageResponse (..))
import Hix.Monad (appContext, fatalError, noteFatal)
import Hix.Pretty (showP)

type Regex = IndexedTraversal' Int Text Match

newtype User =
  User { value :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

newtype Password =
  Password { value :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

data UploadConfig =
  UploadConfig {
    verbosity :: Verbosity,
    user :: User,
    password :: Password
  }
  deriving stock (Eq, Show, Generic)

upload ::
  UploadConfig ->
  RepoContext ->
  Maybe Cabal.Username ->
  Maybe Cabal.Password ->
  IsCandidate ->
  [FilePath] ->
  IO ()
upload conf ctx =
#if MIN_VERSION_cabal_install(3,12,0)
    Upload.upload conf.verbosity ctx Nothing
#else
    Upload.upload conf.verbosity ctx
#endif

-- TODO When candidates are uploaded, versions don't have to be written to the repo.
-- Since we're gonna add automatic semver incrementing, we can do that in memory and manipulate the cabal files in a
-- temp dir somewhere, even without asking the user for a version, since there's no danger in publishing candidates.
--
-- TODO Allow token authentication
publishPackage ::
  UploadConfig ->
  SolveFlags ->
  Path Abs File ->
  M ()
publishPackage conf flags sourceTar =
  withRepoContextM conf.verbosity flags.global \ ctx ->
    catchExitCodeM "Upload failed" do
      upload conf ctx (Just user) (Just password) candidate [toFilePath sourceTar]
  where
    user = Cabal.Username (toString conf.user.value)

    password = Cabal.Password (toString conf.password.value)

    candidate = IsPublished

revisions ::
  HackageClient ->
  PackageId ->
  M [Revision]
revisions client package =
  appContext [exon|fetching revisions for #{showP package}|] do
    result <- hackageGet Right client [exon|package/##{showP package}/revisions/|] HackageResponseJson
    fatalHackageRequest result

latestRevision ::
  HackageClient ->
  PackageId ->
  M Revision
latestRevision client package = do
  revs <- revisions client package
  noteFatal "Package has no revisions" (maximumBy (comparing (.number)) revs)

updateRevision :: Word -> GenericPackageDescription -> GenericPackageDescription
updateRevision newRevision =
  packageDescription . customFieldsPD %~ \ old -> new : filter (not . (revKey ==) . fst) old
  where
    new = (revKey, show newRevision)
    revKey = "x-revision"

revisionCabalFile ::
  HackageClient ->
  PackageId ->
  Word ->
  M Text
revisionCabalFile client package rev =
  appContext [exon|fetching revision #{Color.number rev} for #{Color.package package}|] do
    result <- hackageGet Right client [exon|package/##{showP package}/revision/#{show rev}.cabal|] HackageResponseHtml
    fatalHackageRequest result

tagRegex :: Regex
tagRegex = [regex|<[^>]+>([^<]*)</[^>]+>|]

checkPublishErrorMessage :: Text -> M ()
checkPublishErrorMessage html =
  for_ (nonEmpty (takeMessages afterErrorHeadline)) \ err ->
    fatalError [exon|Publishing revision failed: #{Text.intercalate " | " (toList err)}|]
  where
    takeMessages = filter (not . Text.null) . fmap sanitize . takeWhile isParagraph
    sanitize = toText . unescapeHTML . toString . stripTags
    afterErrorHeadline = drop 1 (dropWhile (not . errorHeadline) (Text.lines html))
    isParagraph = Text.isPrefixOf "<p>"
    errorHeadline line = line == "<h2>Errors</h2>"
    stripTags line = line ^. tagRegex . groups . ix 0

publishRevisionTo ::
  HackageClient ->
  PackageId ->
  Path Abs File ->
  M Revision
publishRevisionTo client package cabalFile = do
  pkgDesc <- parseCabalFile cabalFile
  prev <- latestRevision client package
  let newRevision = prev.number + 1
      withRevision =
        toText $
        showGenericPackageDescription (updateRevision newRevision pkgDesc)
      payload =
        [
          ("pkgid", slug),
          ("cabalfile", withRevision),
          ("publish", "true")
        ]
  Log.info [exon|Publishing revision #{show newRevision} for #{showP package} to #{showP client.description}|]
  hackagePostForm Right client [exon|package/#{slug}/#{cabalName}/edit|] payload HackageResponseHtml >>= \case
    Right response -> checkPublishErrorMessage response
    Left reason -> case reason of
      HackageFatal msg -> err msg
      HackageNotFound -> err [exon|Hackage couldn't find the package '#{slug}'|]
      HackageParseError msg -> err msg
  new <- latestRevision client package
  when (new.number <= prev.number) do
    fatalError "Revision wasn't published"
  pure new
  where
    cabalName = pathText (filename cabalFile)
    slug = showP package
    err msg = fatalError [exon|Publishing revision failed: #{msg}|]

-- TODO better return value
publishRevision ::
  NonEmpty HackageClient ->
  PackageId ->
  Path Abs File ->
  M Revision
publishRevision clients package cabalFile =
  appContext [exon|publishing revision for #{Color.package package} from #{Color.path cabalFile}|] do
    NonEmpty.head <$> for clients \ client -> publishRevisionTo client package cabalFile
