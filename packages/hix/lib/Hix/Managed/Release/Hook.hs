module Hix.Managed.Release.Hook where

import qualified Data.Aeson as Aeson
import Data.Aeson (ToJSON)
import Exon (exon)
import Path (Abs, File, Path, toFilePath)
import System.Exit (ExitCode (..))
import System.Process.Typed (proc, readProcessInterleaved, setEnv)

import Hix.Class.Map (nElems)
import qualified Hix.Color as Color
import Hix.Data.Json (JsonPretty (..))
import Hix.Data.Monad (M)
import Hix.Data.PackageName (LocalPackage)
import Hix.Data.Version (Version)
import Hix.Error (pathText)
import qualified Hix.Log as Log
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Data.ReleaseConfig (ArtifactConfig (..), ReleaseConfig (..))
import Hix.Managed.Release.Data.Staged (UploadingTargetView (..))
import Hix.Managed.Release.Data.UploadResult (hasFailure)
import Hix.Monad (appContext, clientError)
import Hix.Pretty (showP)

data HookPackage =
  HookPackage {
    package :: LocalPackage,
    version :: JsonPretty Version,
    success :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data HookContext =
  HookContext {
    phase :: Text,
    packages :: [HookPackage],
    publish :: ArtifactConfig,
    candidates :: ArtifactConfig
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

hookPackages ::
  Packages UploadingTargetView ->
  [HookPackage]
hookPackages uploading =
  hookPackage <$> nElems uploading
  where
    hookPackage UploadingTargetView {package, releaseVersion, results} =
      HookPackage {package, version = JsonPretty releaseVersion, success = not (hasFailure results)}

hookContext ::
  ReleaseConfig ->
  Text ->
  [HookPackage] ->
  HookContext
hookContext ReleaseConfig {publish, candidates} phase packages =
  HookContext {..}

runHook ::
  [(String, String)] ->
  Path Abs File ->
  M ()
runHook env exe =
  appContext [exon|Running hook #{Color.path (pathText exe)}|] do
    readProcessInterleaved (setEnv env (proc (toFilePath exe) [])) >>= \case
      (ExitSuccess, _) -> unit
      (ExitFailure _, output) -> do
        Log.plain.info (decodeUtf8 output)
        clientError "Hook exited with error status"

runHooks ::
  ReleaseConfig ->
  [HookPackage] ->
  Maybe Version ->
  Text ->
  [Path Abs File] ->
  M ()
runHooks config packages sharedVersion phase hooks =
  appContext [exon|Running hooks for #{Color.yellow phase}|] do
    traverse_ (runHook env) hooks
  where
    env =
      [
        ("context", decodeUtf8 (Aeson.encode context)),
        ("version", foldMap showP sharedVersion),
        ("phase", toString phase)
      ]

    context = hookContext config phase packages

withHooks ::
  ReleaseConfig ->
  [Path Abs File] ->
  Packages UploadingTargetView ->
  Maybe Version ->
  M a ->
  M a
withHooks config hooks uploading sharedVersion run = do
  runHooks config packages sharedVersion "post-upload" hooks
  result <- run
  runHooks config packages sharedVersion "post-commit" hooks
  pure result
  where
    packages = hookPackages uploading
