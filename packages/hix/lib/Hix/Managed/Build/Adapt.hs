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
import qualified Hix.Log as Log
import Hix.Managed.Build.NixOutput (PackageDerivation (..))
import Hix.Managed.Build.NixOutput.Analysis (FailureReason (..), analyzeLog)
import Hix.Managed.Data.StageState (BuildFailure (..), BuildResult (..))

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

catMaybesNonEmpty :: NonEmpty (Maybe a) -> Maybe (NonEmpty a)
catMaybesNonEmpty = nonEmpty . catMaybes . toList

failureCounts :: NonEmpty FailureReason -> FailureCounts
failureCounts pkgs =
  FailureCounts {clear}
  where
    clear = fromIntegral $ length $ flip filter (toList pkgs) \case
      Unclear -> False
      _ -> True

buildAdaptive ::
  (Overrides -> M BuildResult) ->
  (FailureCounts -> PackageDerivation -> Maybe Override -> FailureReason -> M (Maybe RetryPackage)) ->
  StateT (Overrides, Map PackageId Word) M BuildResult
buildAdaptive runBuild suggestOverride = do
  build
  where
    build = do
      result <- lift . runBuild =<< gets fst
      maybe (pure result) retryFailures =<< collectRetries result

    collectRetries = \case
      BuildFailure (PackageFailure pkgs) -> do
        let reasons = [(pkg, analyzeLog pkg.log) | pkg <- pkgs]
            counts = failureCounts (snd <$> reasons)
        results <- traverse (limitOverrides (findOverride counts)) reasons
        pure (catMaybesNonEmpty results)
      _ -> pure Nothing

    retryFailures retries = do
      lift $ Log.verbose [exon|Some packages failed, retrying with overrides: #{ids}|]
      traverse_ (modify' . recordRetry) retries
      build
      where
        ids = Text.intercalate ", " (Color.package <$> toList retries)

    limitOverrides cont (pkg, reason) = do
      count <- gets (\ (_, counts) -> fromMaybe 0 (counts !? pkg.package))
      if count >= 4
      then pure Nothing
      else cont pkg reason

    findOverride counts pkg reason = do
      old <- gets \ (ovs, _) -> ovs !! pkg.package.name
      lift (suggestOverride counts pkg old reason)
