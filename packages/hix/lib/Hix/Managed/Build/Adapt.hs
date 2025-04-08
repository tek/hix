module Hix.Managed.Build.Adapt where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, gets, modify')
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import qualified Data.Text as Text
import Distribution.Pretty (Pretty (..))
import Exon (exon)
import Text.PrettyPrint (brackets, (<+>))

import Hix.Class.Map (nInsert, (!!))
import qualified Hix.Color as Color
import Hix.Data.Monad (M)
import Hix.Data.Overrides (Override (..), Overrides)
import Hix.Data.PackageId (PackageId (..))
import Hix.Data.PackageName (PackageName)
import Hix.Data.Version (Version)
import qualified Hix.Log as Log
import Hix.Managed.Build.NixOutput.Analysis (FailureReason (..), analyzeEarlyFailure, analyzeLog)
import Hix.Managed.Data.NixOutput (PackageDerivation (..))
import Hix.Managed.Data.StageState (BuildFailure (..), BuildResult (..))
import Hix.Maybe (justIf)
import Hix.Monad (appContextT)
import Hix.Pretty (showP)

data FailedPackage =
  FailedPackage {
    package :: PackageName,
    version :: Maybe Version
  }
  deriving stock (Eq, Show)

failedPackageId :: FailedPackage -> Maybe PackageId
failedPackageId FailedPackage {..} =
  version <&> \ v -> PackageId {name = package, version = v}

data RetryPackage =
  RetryPackage {
    package :: PackageId,
    override :: Override
  }
  deriving stock (Eq, Show)

instance Pretty RetryPackage where
  pretty RetryPackage {..} =
    pretty package <+> brackets (shortOverride override)
    where
      shortOverride = \case
        Override {version} -> pretty version
        Jailbreak -> "jailbreak"
        Local -> "local"

data FailureCounts =
  FailureCounts {
    clear :: Word
  }
  deriving stock (Eq, Show)

incrementOrInit :: Maybe Word -> Maybe Word
incrementOrInit = \case
  Just old -> Just (old + 1)
  Nothing -> Just 1

recordRetry :: RetryPackage -> (Overrides, Map PackageId Word) -> (Overrides, Map PackageId Word)
recordRetry retry (overrides, counts) =
  (
    nInsert retry.package.name retry.override overrides,
    Map.alter incrementOrInit retry.package counts
  )

failureCounts :: NonEmpty FailureReason -> FailureCounts
failureCounts pkgs =
  FailureCounts {clear}
  where
    clear = fromIntegral $ length $ flip filter (toList pkgs) \case
      Unclear -> False
      _ -> True

manageableFailures :: BuildResult -> Maybe (NonEmpty (FailedPackage, FailureReason))
manageableFailures = \case
  BuildFailure (PackageFailure pkgs) ->
    Just [
      (FailedPackage {package = name, version = Just version, ..}, analyzeLog log)
      |
      PackageDerivation {package = PackageId {..}, ..} <- pkgs
    ]
  BuildFailure (UnexpectedFailure log) ->
    analyzeEarlyFailure log <&> \ (package, reason) -> pure (FailedPackage {version = Nothing, ..}, reason)
  _ -> Nothing

buildAdaptive ::
  (Overrides -> M BuildResult) ->
  (FailureCounts -> FailedPackage -> Maybe Override -> FailureReason -> M (Maybe RetryPackage)) ->
  StateT (Overrides, Map PackageId Word) M BuildResult
buildAdaptive runBuild suggestOverride = do
  build
  where
    build = do
      result <- lift . runBuild =<< gets fst
      collectRetries result >>= \case
        Just (Just retries) -> retryFailures retries
        Just Nothing -> result <$ lift (Log.verbose "Could not fix any failures heuristically")
        Nothing -> pure result

    collectRetries failure =
      for (manageableFailures failure) \ reasons -> do
        let counts = failureCounts (snd <$> reasons)
        potentialOverrides <- catMaybes <$> traverse (findOverride counts) (toList reasons)
        overrides <- catMaybes <$> traverse limitOverrides potentialOverrides
        pure (nonEmpty overrides)

    retryFailures :: NonEmpty RetryPackage -> StateT (Overrides, Map PackageId Word) M BuildResult
    retryFailures retries = do
      lift $ Log.verbose [exon|Some packages failed, retrying with overrides: #{ids}|]
      traverse_ (modify' . recordRetry) retries
      build
      where
        ids = Text.intercalate ", " (Color.package <$> toList retries)

    limitOverrides pkg =
      gets (\ (_, counts) -> justIf (fromMaybe 0 (counts !? pkg.package) < 4) pkg)

    findOverride counts (pkg, reason) =
      appContextT [exon|finding an override for #{Color.package pkg.package} (#{showP reason})|] do
        old <- gets \ (ovs, _) -> ovs !! pkg.package
        lift (suggestOverride counts pkg old reason)
