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
import Distribution.Client.Types (RemoteRepo (..), Repo (RepoSecure), RepoName (RepoName))
import qualified Distribution.Client.Types.Repo
import Distribution.Verbosity (Verbosity)
import Exon (exon)
import Path (Abs, File, Path, addExtension, parseAbsFile)

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
import Hix.Monad (M, catchIOM, eitherFatalShow, noteFatal, tryIOM, tryIOMWith, withLower)
import Hix.Pretty (showP)

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
  SolveConfig ->
  String
updateRequest SolveConfig {hackageRepoName, cabal = CabalConfig {indexState}} =
  [exon|#{hackage}#{extra}|]
  where
    extra = foldMap stateField (timestampToUTCTime . coerce =<< indexState)

    stateField UTCTime {..} = [exon|,#{showGregorian utctDay}T#{show (timeToTimeOfDay utctDayTime)}Z|]

    HackageRepoName (toString -> hackage) = hackageRepoName

maxIndexAgeDays :: Int
maxIndexAgeDays = 7

maxIndexAge :: NominalDiffTime
maxIndexAge = nominalDay * fromIntegral maxIndexAgeDays

updateIndex ::
  SolveConfig ->
  GlobalFlags ->
  NixStyleFlags () ->
  IndexProblem ->
  M ()
updateIndex conf global main problem = do
  Log.verbose [exon|Hackage snapshot #{message}, fetching...|]
  tryIOMWith (\ err -> Fatal [exon|Fetching Hackage snapshot failed: #{err}|]) do
    updateAction main [updateRequest conf] global
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
    err = "Bad Hackage repo config"

currentIndexState ::
  Verbosity ->
  RepoContext ->
  Repo ->
  M (Maybe HackageIndexState)
currentIndexState verbosity ctx repo =
  catchIOM (Just . HackageIndexState <$> currentIndexTimestamp verbosity ctx repo) (const (pure Nothing))

indexProblem ::
  SolveConfig ->
  RepoContext ->
  Repo ->
  Path Abs File ->
  M (Either IndexProblem ValidIndex)
indexProblem SolveConfig {verbosity, cabal} ctx repo path = do
  now <- tryIOM getCurrentTime
  Log.debug [exon|Checking Hackage snapshot at #{pathText path}|]
  currentIndexState verbosity ctx repo <&> \case
    -- There is no index, so the target is irrelevant.
    Nothing -> Left IndexMissing
    Just currentState
      -- If a target index state was specified, we only care whether it matches exactly.
      -- The later conditions are irrelevant.
      | Just target <- cabal.indexState
      -> if currentState == target
         then Right (IndexMatch target)
         else Left IndexMismatch
      -- If no target was specified, the age of the current index must be below the threshold.
      -- The guard expresses that the timestamp could be parsed.
      | Just current <- currentUTC
      , let age = diffUTCTime now current
      -> if diffUTCTime now current > maxIndexAge
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
ensureHackageIndex conf global main =
  withRepoContextM conf global \ ctx -> do
    repo <- fullHackageRepo conf ctx
    path <- indexPath repo
    either (updateIndex conf global main) logValid =<< indexProblem conf ctx repo path
