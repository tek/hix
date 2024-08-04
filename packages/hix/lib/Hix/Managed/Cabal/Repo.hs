{-# language CPP #-}

module Hix.Managed.Cabal.Repo where

import Data.Time (
  NominalDiffTime,
  UTCTime (..),
  diffUTCTime,
  getCurrentTime,
  nominalDay,
  showGregorian,
  timeToTimeOfDay,
  )
import Distribution.Client.CmdUpdate (updateAction)
import qualified Distribution.Client.GlobalFlags
import Distribution.Client.GlobalFlags (GlobalFlags (..))
import Distribution.Client.IndexUtils (currentIndexTimestamp, indexBaseName)
import Distribution.Client.IndexUtils.Timestamp (Timestamp, timestampToUTCTime)
import Distribution.Client.NixStyleOptions (NixStyleFlags (..))
import Distribution.Client.Setup (RepoContext, withRepoContext)
import Distribution.Client.Types (RemoteRepo (..), Repo (RepoRemote, RepoSecure), RepoName (RepoName))
import qualified Distribution.Client.Types.Repo
import Distribution.Verbosity (Verbosity)
import Exon (exon)
import Path (Abs, File, Path, addExtension, parseAbsFile)

import Hix.Data.Error (ErrorMessage (Fatal))
import Hix.Error (pathText)
import qualified Hix.Log as Log
import qualified Hix.Managed.Cabal.Data.Config
import Hix.Managed.Cabal.Data.Config (SolveConfig (SolveConfig))
import Hix.Managed.Cabal.Data.HackageRepo (HackageIndexState (..), HackageName (..), HackageRepo (..))
import Hix.Monad (M, catchIOM, eitherFatalShow, noteFatal, tryIOM, tryIOMWith, withLower)
import Hix.Pretty (prettyV, showP)

#if MIN_VERSION_cabal_install(3,12,0)
import Distribution.Client.IndexUtils (Index (RepoIndex))
#endif

withRepoContextM ::
  Verbosity ->
  GlobalFlags ->
  (RepoContext -> M a) ->
  M a
withRepoContextM verbosity flags f =
  withLower \ lower -> withRepoContext verbosity flags \ ctx -> lower (f ctx)

fullHackageRepos ::
  SolveConfig ->
  RepoContext ->
  M (NonEmpty (HackageRepo, Repo))
fullHackageRepos SolveConfig {hackageRepos} ctx =
  for hackageRepos \ repo@HackageRepo {name = HackageName (toString -> name)} -> do
    let matchName RemoteRepo {remoteRepoName = RepoName cabalName} = cabalName == name
    cabalRepo <- noteFatal (err name) $ flip find ctx.repoContextRepos \case
      RepoSecure {repoRemote} -> matchName repoRemote
      RepoRemote {repoRemote} -> matchName repoRemote
      _ -> False
    pure (repo, cabalRepo)
  where
    err name = [exon|Configured repo '#{toText name}' not found in Cabal flags|]

data IndexProblem =
  IndexMissing
  |
  IndexOutdated NominalDiffTime
  |
  IndexMismatch
  |
  IndexCorrupt Timestamp
  deriving stock (Eq, Show, Generic)

data ValidIndex =
  IndexMatch HackageIndexState
  |
  IndexRecent NominalDiffTime
  deriving stock (Eq, Show, Generic)

updateRequest ::
  HackageRepo ->
  String
updateRequest HackageRepo {name, indexState} =
  [exon|##{name}#{extra}|]
  where
    extra = foldMap stateField (timestampToUTCTime . coerce =<< indexState)

    stateField UTCTime {..} = [exon|,#{showGregorian utctDay}T#{show (timeToTimeOfDay utctDayTime)}Z|]

maxIndexAgeDays :: Int
maxIndexAgeDays = 7

maxIndexAge :: NominalDiffTime
maxIndexAge = nominalDay * fromIntegral maxIndexAgeDays

updateIndex ::
  HackageRepo ->
  GlobalFlags ->
  NixStyleFlags () ->
  IndexProblem ->
  M ()
updateIndex repo@HackageRepo {name} global main problem = do
  Log.verbose [exon|Hackage snapshot for '##{name}' #{message}, fetching...|]
  tryIOMWith (\ err -> Fatal [exon|Fetching Hackage snapshot failed: #{err}|]) do
    updateAction main [updateRequest repo] global
  where
    message = case problem of
      IndexMissing -> "doesn't exist"
      IndexOutdated age -> [exon|is older than #{show maxIndexAgeDays} days (#{show age})|]
      IndexMismatch -> "differs from requested index state"
      IndexCorrupt stamp -> [exon|has corrupt timestamp (#{show stamp})|]

indexPath :: Repo -> M (Path Abs File)
indexPath repo =
  eitherFatalShow err do
    base <- parseAbsFile (indexBaseName repo)
    addExtension ".tar" base
  where
    err = "Bad Hackage index file name"

currentIndexState ::
  Verbosity ->
  RepoContext ->
  Repo ->
  M (Maybe HackageIndexState)
currentIndexState verbosity ctx repo =
#if MIN_VERSION_cabal_install(3,12,0)
  catchIOM (Just . HackageIndexState <$> currentIndexTimestamp verbosity (RepoIndex ctx repo)) (const (pure Nothing))
#else
  catchIOM (Just . HackageIndexState <$> currentIndexTimestamp verbosity ctx repo) (const (pure Nothing))
#endif

indexProblem ::
  SolveConfig ->
  RepoContext ->
  HackageRepo ->
  Repo ->
  Path Abs File ->
  M (Either IndexProblem ValidIndex)
indexProblem SolveConfig {verbosity} ctx repo cabalRepo path = do
  now <- tryIOM getCurrentTime
  Log.debug [exon|Checking Hackage snapshot at #{pathText path}|]
  currentIndexState verbosity ctx cabalRepo <&> \case
    -- There is no index, so the target is irrelevant.
    Nothing -> Left IndexMissing
    Just currentState
      -- If a target index state was specified, we only care whether it matches exactly.
      -- The later conditions are irrelevant.
      | Just target <- repo.indexState
      -> if currentState == target
         then Right (IndexMatch target)
         else Left IndexMismatch
      -- If no target was specified, the age of the current index must be below the threshold.
      -- The guard expresses that the timestamp could be parsed.
      | Just current <- currentUTC
      , let age = diffUTCTime now current
      -> if age > maxIndexAge
         then Left (IndexOutdated age)
         else Right (IndexRecent age)
      -- If the current state's timestamp can't be converted to UTCTime, it may be corrupt, so we update.
      | otherwise
      -> Left (IndexCorrupt (coerce currentState))
      where
        currentUTC = timestampToUTCTime (coerce currentState)

logValid ::
  ValidIndex ->
  M ()
logValid = \case
  IndexMatch (HackageIndexState ts) -> Log.debug [exon|Snapshot matches target: #{showP ts}|]
  IndexRecent age ->
    Log.debug [exon|Snapshot is less than #{desc} old (maximum #{show maxIndexAgeDays}).|]
    where
      desc | days == 1 = "a day"
           | otherwise = [exon|#{show days} days|]
      days :: Int
      days = maybe 0 ceiling (age / nominalDay)

-- TODO currentIndexTimestamp has a different signature in later versions
ensureHackageIndex ::
  SolveConfig ->
  GlobalFlags ->
  NixStyleFlags () ->
  M ()
ensureHackageIndex conf global main = do
  Log.debug [exon|Ensuring Hackage indexes for repos: #{showP (prettyV conf.hackageRepos)}|]
  withRepoContextM conf.verbosity global \ ctx -> do
    fullHackageRepos conf ctx >>= traverse_ \ (repo, cabalRepo) -> do
      path <- indexPath cabalRepo
      either (updateIndex repo global main) logValid =<< indexProblem conf ctx repo cabalRepo path
