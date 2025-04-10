module Hix.Path where

import Control.Monad.Trans.Reader (ask)
import Exon (exon)
import Path (Abs, Dir, File, Path, parent, toFilePath)
import Path.IO (resolveDir, resolveFile)
import System.FilePattern.Directory (getDirectoryFiles)
import System.IO.Error (tryIOError)

import qualified Hix.Data.Monad
import Hix.Data.Monad (AppResources (AppResources), M (M), liftE)
import qualified Hix.Data.PathSpec as PathSpec
import Hix.Data.PathSpec (PathSpec)
import Hix.Monad (appContext)
import Hix.Pretty (showHP)

findFlake :: Path Abs Dir -> IO (Maybe (Path Abs Dir))
findFlake cur =
  tryIOError (getDirectoryFiles (toFilePath cur) ["flake.nix"]) >>= either (const (pure Nothing)) \case
    [_] -> pure (Just cur)
    _ | parent cur == cur -> pure Nothing
    _ | otherwise -> findFlake (parent cur)

inferRoot :: M (Path Abs Dir)
inferRoot = do
  AppResources {cwd} <- M ask
  fromMaybe cwd <$> liftIO (findFlake cwd)

rootDir :: Maybe (Path Abs Dir) -> M (Path Abs Dir)
rootDir =
  maybe inferRoot pure

resolvePathSpecWith ::
  (Path Abs Dir -> FilePath -> IO (Path Abs t)) ->
  PathSpec t ->
  M (Path Abs t)
resolvePathSpecWith resolver pathSpec =
  appContext [exon|resolving path spec #{showHP pathSpec}|] $ do
    AppResources {cwd} <- M ask
    liftE $ PathSpec.resolvePathSpec resolver cwd pathSpec

class PathSpecResolver t where
  resolvePathSpec :: PathSpec t -> M (Path Abs t)

instance PathSpecResolver Dir where
  resolvePathSpec = resolvePathSpecWith resolveDir

instance PathSpecResolver File where
  resolvePathSpec = resolvePathSpecWith resolveFile
