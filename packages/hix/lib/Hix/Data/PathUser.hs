module Hix.Data.PathUser where

import Control.Monad.Trans.Reader (ask)
import Path (Abs, Dir, File, Path)
import Path.IO (resolveDir, resolveFile)

import qualified Hix.Data.Monad
import Hix.Data.Monad (AppResources (AppResources), M (M))
import Hix.Monad (tryIOM)

newtype PathUser = PathUser { unPathUser :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString)

resolvePathUserDir :: PathUser -> M (Path Abs Dir)
resolvePathUserDir (PathUser path) = do
  AppResources {cwd} <- M ask
  tryIOM $ resolveDir cwd (toString path)

resolvePathUserFile :: PathUser -> M (Path Abs File)
resolvePathUserFile (PathUser path) = do
  AppResources {cwd} <- M ask
  tryIOM $ resolveFile cwd (toString path)
