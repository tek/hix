module Hix.Managed.Handlers.Build.Prod where

import Control.Monad.Catch (catch)
import Control.Monad.Trans.Reader (asks)
import Distribution.Client.Dependency (DepResolverParams)
import Exon (exon)
import Path (Abs, Dir, Path, toFilePath)
import Path.IO (copyDirRecur', getCurrentDir)
import System.IO.Error (IOError)
import System.Process.Typed (
  ExitCode (ExitFailure, ExitSuccess),
  inherit,
  nullStream,
  proc,
  runProcess,
  setStderr,
  setStdout,
  setWorkingDir,
  )

import Hix.Data.EnvName (EnvName)
import Hix.Data.Error (Error (Fatal))
import qualified Hix.Data.Monad (Env (debug, verbose))
import Hix.Data.Package (LocalPackage)
import qualified Hix.Data.Version
import Hix.Data.Version (NewVersion (NewVersion))
import Hix.Error (pathText)
import qualified Hix.Log as Log
import Hix.Managed.Handlers.Build (BuildHandlers (..), TempProjectBracket (TempProjectBracket))
import qualified Hix.Managed.Handlers.Hackage.Prod as HackageHandlers
import qualified Hix.Managed.Handlers.Solve as SolveHandlers
import qualified Hix.Managed.Handlers.Solve.Prod as SolveHandlers
import qualified Hix.Managed.Handlers.StateFile.Prod as StateFileHandlers
import Hix.Monad (M, throwM, tryIOM, withTempDir)
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

buildProject ::
  Path Abs Dir ->
  EnvName ->
  LocalPackage ->
  NewVersion ->
  M Bool
buildProject root env target NewVersion {package, version} = do
  verbose <- asks (.verbose)
  ifM (asks (.debug)) logFull logBasic
  tryIOM (runProcess (conf verbose)) <&> \case
    ExitSuccess -> True
    ExitFailure _ -> False
  where
    conf verbose = err verbose (setWorkingDir (toFilePath root) (proc "nix" args))

    err = \case
      True -> setStderr inherit . setStdout inherit
      False -> setStderr nullStream

    logBasic = Log.info [exon|Building '##{target}' with '#{pv}'...|]

    logFull = Log.debug [exon|Building '##{target}' for '#{pv}' at #{pathText root} with args #{show args}|]

    pv = [exon|##{package}-#{showP version}|]

    args = ["-L", "build", [exon|path:#{".#"}env.##{env}.##{target}|]]

handlersProdNoSolver :: IO BuildHandlers
handlersProdNoSolver = do
  hackage <- HackageHandlers.handlersProd
  pure BuildHandlers {
    stateFile = StateFileHandlers.handlersProd,
    solve = SolveHandlers.handlersNull,
    hackage,
    withTempProject = TempProjectBracket withTempProject,
    buildProject
  }

handlersProd ::
  (DepResolverParams -> DepResolverParams) ->
  Maybe (Path Abs Dir) ->
  M BuildHandlers
handlersProd solverParams ghc = do
  handlers <- liftIO handlersProdNoSolver
  solve <- SolveHandlers.handlersProd solverParams ghc
  pure handlers {solve}
