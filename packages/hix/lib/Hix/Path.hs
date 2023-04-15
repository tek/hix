module Hix.Path where

import Control.Monad.Trans.Reader (ask)
import Path (Abs, Dir, Path, parent, toFilePath)
import System.FilePattern.Directory (getDirectoryFiles)
import System.IO.Error (tryIOError)

import qualified Hix.Monad
import Hix.Monad (Env (Env), M)

findFlake :: Path Abs Dir -> IO (Maybe (Path Abs Dir))
findFlake cur =
  tryIOError (getDirectoryFiles (toFilePath cur) ["flake.nix"]) >>= either (const (pure Nothing)) \case
    [_] -> pure (Just cur)
    _ | parent cur == cur -> pure Nothing
    _ | otherwise -> findFlake (parent cur)

inferRoot :: M (Path Abs Dir)
inferRoot = do
  Env {cwd} <- ask
  fromMaybe cwd <$> liftIO (findFlake cwd)

rootDir :: Maybe (Path Abs Dir) -> M (Path Abs Dir)
rootDir =
  maybe inferRoot pure
