module Hix.Managed.Handlers.StateFile.Prod where

import qualified Data.Text.IO as Text
import Path (Abs, Dir, File, Path, Rel, parent, toFilePath, (</>))
import Path.IO (createDirIfMissing, doesDirExist, doesFileExist)
import System.Posix (fileMode, getFileStatus, ownerWriteMode, setFileMode, unionFileModes)

import Hix.Data.NixExpr (Expr)
import Hix.Managed.Handlers.StateFile (StateFileHandlers (..))
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

initFile :: Path Abs Dir -> Path Rel File -> M (Path Abs File)
initFile root file = do
  createDirIfMissing False (parent depsFile)
  setDepsFileWritable depsFile
  pure depsFile
  where
    depsFile = root </> file

writeFile ::
  Path Abs File ->
  Expr ->
  M ()
writeFile depsFile nixExpr =
  liftIO (Text.writeFile (toFilePath depsFile) (renderRootExpr nixExpr))

handlersProd :: StateFileHandlers
handlersProd =
  StateFileHandlers {
    initFile,
    writeFile
  }
