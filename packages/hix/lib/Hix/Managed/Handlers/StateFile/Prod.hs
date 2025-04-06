module Hix.Managed.Handlers.StateFile.Prod where

import qualified Data.Text.IO as Text
import Path (Abs, Dir, File, Path, parent, toFilePath)
import Path.IO (createDirIfMissing, doesDirExist, doesFileExist, resolveFile)
import System.Posix (fileMode, getFileStatus, ownerWriteMode, setFileMode, unionFileModes)

import qualified Hix.Data.Monad
import Hix.Data.Monad (appRes, liftE)
import Hix.Data.NixExpr (Expr)
import Hix.Data.PathSpec (resolvePathSpec)
import qualified Hix.Managed.Data.StateFileConfig
import Hix.Managed.Data.StateFileConfig (StateFileConfig)
import Hix.Managed.Handlers.StateFile (StateFileHandlers (..))
import Hix.Maybe (fromMaybeA)
import Hix.Monad (M, tryIOM)
import Hix.NixExpr (renderRootExpr)

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

initFile ::
  StateFileConfig ->
  Maybe (Path Abs Dir) ->
  M (Path Abs File)
initFile conf tmpRoot = do
  root <- fromMaybeA appRes.root tmpRoot
  depsFile <- liftE $ resolvePathSpec resolveFile root conf.file
  createDirIfMissing False (parent depsFile)
  setDepsFileWritable depsFile
  pure depsFile

writeFile ::
  StateFileConfig ->
  Maybe (Path Abs Dir) ->
  Expr ->
  M ()
writeFile conf tmpRoot nixExpr = do
  path <- initFile conf tmpRoot
  liftIO (Text.writeFile (toFilePath path) (renderRootExpr nixExpr))

handlersProd ::
  StateFileConfig ->
  StateFileHandlers
handlersProd conf =
  StateFileHandlers {writeFile = writeFile conf}
