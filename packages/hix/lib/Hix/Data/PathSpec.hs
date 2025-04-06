module Hix.Data.PathSpec where

import Control.Monad.Trans.Except (ExceptT)
import Path (Abs, Dir, Path, Rel, SomeBase (Abs, Rel), (</>))
import Path.IO (AnyPath (AbsPath, makeAbsolute))

import Hix.Data.Error (Error)
import Hix.Error (tryIO)

data PathSpec t = PathConcrete (SomeBase t) | PathUser Text
  deriving stock (Eq, Show)

instance IsString (PathSpec t) where
  fromString str = PathUser (fromString str)

resolvePathSpec ::
  (Path Abs Dir -> FilePath -> IO (Path Abs t)) ->
  Path Abs Dir ->
  PathSpec t ->
  ExceptT Error IO (Path Abs t)
resolvePathSpec resolver cwd = \case
  PathConcrete path -> case path of
    Abs a -> pure a
    Rel r -> pure $ cwd </> r
  PathUser path -> tryIO $ resolver cwd (toString path)

resolvePathSpec' ::
  (Path Abs t ~ AbsPath (Path Rel t)) =>
  (AnyPath (Path Rel t)) =>
  (FilePath -> IO (Path Abs t)) ->
  PathSpec t ->
  ExceptT Error IO (Path Abs t)
resolvePathSpec' resolver = \case
  PathConcrete path -> case path of
    Abs a -> pure a
    Rel r -> tryIO $ makeAbsolute r
  PathUser path -> tryIO $ resolver (toString path)
