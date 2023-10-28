module Hix.Bump.Handlers.Prod where

import Control.Monad.Catch (catch)
import Control.Monad.Trans.Reader (asks)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Distribution.Pretty (pretty)
import Exon (exon)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Path (Abs, Dir, File, Path, Rel, parent, toFilePath, (</>))
import Path.IO (copyDirRecur', createDirIfMissing, doesDirExist, doesFileExist, getCurrentDir, withSystemTempDir)
import System.IO.Error (IOError)
import System.Posix (fileMode, getFileStatus, ownerWriteMode, setFileMode, unionFileModes)
import System.Process.Typed (
  ExitCode (ExitFailure, ExitSuccess),
  inherit,
  nullStream,
  proc,
  readProcess,
  runProcess,
  setStderr,
  setWorkingDir,
  )

import qualified Hix.Console as Console
import qualified Hix.Data.BumpHandlers
import Hix.Data.BumpHandlers (BuildResults, BumpHandlers (BumpHandlers), TempProjectBracket (TempProjectBracket))
import Hix.Data.ComponentConfig (EnvName, PackageName)
import Hix.Data.Error (Error (BumpError, Fatal), pathText)
import qualified Hix.Data.Version
import Hix.Data.Version (
  NewRange (NewRange),
  SourceHash (SourceHash),
  VersionBump (VersionBump),
  VersionBumped (VersionBumped),
  )
import Hix.Hackage (latestVersionHackage)
import qualified Hix.Monad
import Hix.Monad (M, logDebug, throwM, tryIOM)
import Hix.NixExpr (Expr, renderRootExpr)

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
      withSystemTempDir "hix-bump-XXXX" \ tmpRoot -> do
        copyDirRecur' projectRoot tmpRoot
        use tmpRoot
    \ (err :: IOError) -> throwM (Fatal (show err))

buildProject ::
  Path Abs Dir ->
  EnvName ->
  PackageName ->
  VersionBump ->
  M Bool
buildProject root env pkg bump = do
  verbose <- asks (.verbose)
  ifM (asks (.debug)) logFull logBasic
  tryIOM (runProcess (conf verbose)) <&> \case
    ExitSuccess -> True
    ExitFailure _ -> False
  where
    conf verbose = err verbose (setWorkingDir (toFilePath root) (proc "nix" args))

    err = \case
      True -> setStderr inherit
      False -> setStderr nullStream

    logBasic = Console.info [exon|Building '##{pkg}' with '##{bpkg}-#{show (pretty version)}'|]

    logFull = logDebug [exon|Building '##{pkg}' for '#{show bump}' at #{pathText root} with args #{show args}|]

    args = ["-L", "build", [exon|#{".#"}env.##{env}.##{pkg}|]]

    version = bump.newVersion

    bpkg = bump.package

setDepsFileWritable ::
  Path Abs File ->
  M ()
setDepsFileWritable file =
  tryIOM do
    whenM (doesDirExist dir) do
      whenM (doesFileExist file) do
        fileCur <- getFileStatus fileFp
        setWrite fileFp (fileMode fileCur)
  where
    setWrite path cur = setFileMode path (unionFileModes cur ownerWriteMode)
    fileFp = toFilePath file
    dir = parent file

initDepsFile :: Path Abs Dir -> Path Rel File -> M (Path Abs File)
initDepsFile root file = do
  createDirIfMissing False (parent depsFile)
  setDepsFileWritable depsFile
  pure depsFile
  where
    depsFile = root </> file

fetchHash ::
  VersionBump ->
  M VersionBumped
fetchHash bump = do
  logDebug [exon|Fetching hash for '##{pkg}' from ##{url}|]
  tryIOM (readProcess conf) >>= \case
    (ExitFailure _, _, err) ->
      throwM (BumpError [exon|Prefetching source of '##{pkg}' from hackage failed: #{decodeUtf8 err}|])
    (ExitSuccess, hash, _) ->
      pure VersionBumped {bump, hash = SourceHash (Text.stripEnd (decodeUtf8 hash))}
  where
    conf = proc "nix-prefetch-url" ["--unpack", url]
    url = [exon|https://hackage.haskell.org/package/#{name}/#{name}.tar.gz|]
    version = show (pretty bump.newVersion)
    name = [exon|##{pkg}-#{version}|]
    pkg = bump.package

writeDepsFile ::
  Path Abs File ->
  Expr ->
  M ()
writeDepsFile depsFile nixExpr =
  liftIO (Text.writeFile (toFilePath depsFile) (renderRootExpr nixExpr))

reportBumps ::
  PackageName ->
  BuildResults ->
  M ()
reportBumps pkg results = do
  for_ (nonEmpty results.success) \ success -> do
    Console.info [exon|Updated dependency versions of '##{pkg}':|]
    for_ ((.bump) <$> success) \ p ->
      Console.out (listPkg p)
  for_ (nonEmpty results.failed) \ failed -> do
    Console.error [exon|Some dependency version updates of '##{pkg}' caused the build to fail:|]
    for_ failed \ p ->
      Console.err (listPkg p)
  when results.initial do
    Console.info [exon|You can remove the version bounds for '##{pkg}' from the flake config.|]
  where
    listPkg VersionBump {package, range, newVersion} =
      [exon| 📦 ##{package} #{show (pretty newVersion)} [#{listRange range}]|]
    listRange = \case
      Just (NewRange range) -> show (pretty range)
      Nothing -> "old range matches"

prodHandlers :: IO BumpHandlers
prodHandlers = do
  manager <- newManager tlsManagerSettings
  pure BumpHandlers {
    latestVersion = latestVersionHackage manager,
    withTempProject = TempProjectBracket withTempProject,
    buildProject,
    initDepsFile,
    fetchHash,
    writeDepsFile,
    reportBumps
    }
