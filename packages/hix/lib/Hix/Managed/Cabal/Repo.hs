module Hix.Managed.Cabal.Repo where

import Data.Time (UTCTime (..), showGregorian, timeToTimeOfDay)
import Distribution.Client.CmdUpdate (updateAction)
import qualified Distribution.Client.GlobalFlags
import Distribution.Client.GlobalFlags (GlobalFlags (..))
import Distribution.Client.IndexUtils (currentIndexTimestamp, indexBaseName)
import Distribution.Client.IndexUtils.Timestamp (timestampToUTCTime)
import Distribution.Client.NixStyleOptions (NixStyleFlags (..))
import Distribution.Client.Setup (RepoContext, withRepoContext)
import Distribution.Client.Types (RemoteRepo (..), Repo (RepoSecure), RepoName (RepoName))
import qualified Distribution.Client.Types.Repo
import Distribution.Compat.Time (getFileAge)
import Distribution.Verbosity (Verbosity)
import Exon (exon)
import Path (Abs, File, Path, addExtension, parseAbsFile, toFilePath)
import Path.IO (doesFileExist)

import Hix.Data.Error (Error (Fatal))
import Hix.Error (pathText)
import qualified Hix.Log as Log
import qualified Hix.Managed.Cabal.Data.Config
import Hix.Managed.Cabal.Data.Config (
  CabalConfig (CabalConfig),
  HackageIndexState (HackageIndexState),
  HackageRepoName (HackageRepoName),
  SolveConfig (SolveConfig),
  )
import Hix.Maybe (justIf)
import Hix.Monad (M, catchIOM, eitherFatalShow, noteFatal, tryIOM, tryIOMWith, withLower)

withRepoContextM ::
  SolveConfig ->
  GlobalFlags ->
  (RepoContext -> M a) ->
  M a
withRepoContextM conf flags f =
  withLower \ lower -> withRepoContext conf.verbosity flags \ ctx -> lower (f ctx)

fullHackageRepo ::
  SolveConfig ->
  RepoContext ->
  M Repo
fullHackageRepo SolveConfig {hackageRepoName = HackageRepoName (toString -> hackage)} ctx =
  noteFatal err $ flip find ctx.repoContextRepos \case
    RepoSecure {repoRemote = RemoteRepo {remoteRepoName = RepoName name}} -> name == hackage
    _ -> False
  where
    err = "Bad Hackage repo config"

data IndexProblem =
  IndexMissing
  |
  IndexOutdated
  |
  IndexMismatch
  deriving stock (Eq, Show, Generic)

indexOutdated :: Path Abs File -> IO Bool
indexOutdated (toFilePath -> index) =
  (> 7) <$> getFileAge index

updateRequest ::
  SolveConfig ->
  String
updateRequest SolveConfig {hackageRepoName, cabal = CabalConfig {indexState}} =
  [exon|#{hackage}#{extra}|]
  where
    extra = foldMap stateField (timestampToUTCTime . coerce =<< indexState)

    stateField UTCTime {..} = [exon|,#{showGregorian utctDay}T#{show (timeToTimeOfDay utctDayTime)}Z|]

    HackageRepoName (toString -> hackage) = hackageRepoName

updateIndex ::
  SolveConfig ->
  GlobalFlags ->
  NixStyleFlags () ->
  IndexProblem ->
  M ()
updateIndex conf global main problem = do
  Log.verbose [exon|Hackage snapshot #{message}, fetching...|]
  tryIOMWith (\ err -> Fatal [exon|Fetching hackage snapshot failed: #{err}|]) do
    updateAction main [updateRequest conf] global
  where
    message = case problem of
      IndexMissing -> "doesn't exist"
      IndexOutdated -> "is older than 7 days"
      IndexMismatch -> "differs from requested index state"

indexPath :: Repo -> M (Path Abs File)
indexPath repo =
  eitherFatalShow err do
    base <- parseAbsFile (indexBaseName repo)
    addExtension ".tar" base
  where
    err = "Bad Hackage repo config"

matchIndexState ::
  Verbosity ->
  RepoContext ->
  Repo ->
  HackageIndexState ->
  M (Maybe IndexProblem)
matchIndexState verbosity ctx repo (HackageIndexState target) =
  catchIOM (match <$> currentIndexTimestamp verbosity ctx repo) (\ _ -> pure (Just IndexMissing))
  where
    match current | current == target = Nothing
                  | otherwise = Just IndexMismatch

indexProblem ::
  SolveConfig ->
  RepoContext ->
  Repo ->
  Path Abs File ->
  M (Maybe IndexProblem)
indexProblem SolveConfig {verbosity, cabal} ctx repo path = do
  Log.debug [exon|Checking hackage snapshot at #{pathText path}|]
  tryIOM (doesFileExist path) >>= \case
    False -> pure (Just IndexMissing)
    True
      | Just target <- cabal.indexState
      -> matchIndexState verbosity ctx repo target
      | otherwise
      -> flip justIf IndexOutdated <$> tryIOM (indexOutdated path)

-- TODO currentIndexTimestamp has a different signature in later versions
ensureHackageIndex ::
  SolveConfig ->
  GlobalFlags ->
  NixStyleFlags () ->
  M ()
ensureHackageIndex conf global main =
  withRepoContextM conf global \ ctx -> do
    repo <- fullHackageRepo conf ctx
    path <- indexPath repo
    traverse_ (updateIndex conf global main) =<< indexProblem conf ctx repo path
