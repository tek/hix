module Hix.Managed.Handlers.Build.Prod where

import Control.Monad.Catch (catch)
import Control.Monad.Trans.Reader (asks)
import Exon (exon)
import Path (Abs, Dir, Path, parseAbsDir, toFilePath)
import Path.IO (copyDirRecur', getCurrentDir)
import System.IO.Error (IOError)
import System.Process.Typed (
  ExitCode (ExitFailure, ExitSuccess),
  ProcessConfig,
  inherit,
  nullStream,
  proc,
  readProcessStdout,
  runProcess,
  setStderr,
  setStdout,
  setWorkingDir,
  )

import Hix.Data.EnvName (EnvName)
import Hix.Data.Error (Error (Fatal))
import qualified Hix.Data.Monad (Env (verbose))
import Hix.Data.Package (LocalPackage)
import qualified Hix.Data.Version
import Hix.Data.Version (NewVersion (NewVersion))
import Hix.Error (pathText)
import qualified Hix.Log as Log
import Hix.Managed.Handlers.Build (BuildHandlers (..), TempProjectBracket (TempProjectBracket))
import qualified Hix.Managed.Handlers.Hackage.Prod as HackageHandlers
import qualified Hix.Managed.Handlers.StateFile.Prod as StateFileHandlers
import Hix.Monad (M, noteFatal, throwM, tryIOM, withTempDir)
import Hix.Pretty (showP)

rootOrCwd ::
  Maybe (Path Abs Dir) ->
  M (Path Abs Dir)
rootOrCwd =
  maybe (tryIOM getCurrentDir) pure

withTempProject ::
  Maybe (Path Abs Dir) ->
  (Path Abs Dir -> M a) ->
  M a
withTempProject rootOverride use = do
  projectRoot <- rootOrCwd rootOverride
  catch
    do
      withTempDir "managed-build" \ tmpRoot -> do
        copyDirRecur' projectRoot tmpRoot
        use tmpRoot
    \ (err :: IOError) -> throwM (Fatal (show err))

nixProc ::
  Path Abs Dir ->
  [Text] ->
  Text ->
  [Text] ->
  M (ProcessConfig () () ())
nixProc root cmd installable extra = do
  Log.debug [exon|Running nix at '#{pathText root}' with args #{show args}|]
  conf <$> asks (.verbose)
  where
    conf verbose = err verbose (setWorkingDir (toFilePath root) (proc "nix" args))

    err = \case
      True -> setStderr inherit
      False -> setStderr nullStream

    args = toString <$> cmd ++ [exon|path:#{".#"}#{installable}|] : extra

buildProject ::
  Path Abs Dir ->
  EnvName ->
  LocalPackage ->
  NewVersion ->
  M Bool
buildProject root env target NewVersion {package, version} = do
  verbose <- asks (.verbose)
  Log.info [exon|Building '##{target}' with '#{pv}'...|]
  conf <- nixProc root ["-L", "build"] [exon|env.##{env}.##{target}|] []
  tryIOM (runProcess (err verbose conf)) <&> \case
    ExitSuccess -> True
    ExitFailure _ -> False
  where
    err = \case
      True -> setStdout inherit
      False -> id

    pv = [exon|##{package}-#{showP version}|]

ghcDb ::
  Maybe Text ->
  Path Abs Dir ->
  EnvName ->
  M (Maybe (Path Abs Dir))
ghcDb buildOutputsPrefix root env = do
  conf <- nixProc root ["eval", "--raw"] [exon|#{prefix}.env.##{env}.ghc-local|] []
  tryIOM (readProcessStdout conf) >>= \case
    (ExitSuccess, decodeUtf8 -> out) -> Just <$> noteFatal (parseError out) (parseAbsDir out)
    (ExitFailure _, _) -> throwM (Fatal "Evaluation failed for vanilla GHC path")
  where
    prefix = fromMaybe "build" buildOutputsPrefix
    parseError out = [exon|Parse error for vanilla GHC path: #{toText out}|]

handlersProd :: Maybe Text -> IO BuildHandlers
handlersProd buildOutputsPrefix = do
  hackage <- HackageHandlers.handlersProd
  pure BuildHandlers {
    stateFile = StateFileHandlers.handlersProd,
    hackage,
    withTempProject = TempProjectBracket withTempProject,
    buildProject,
    ghcDb = ghcDb buildOutputsPrefix
  }
