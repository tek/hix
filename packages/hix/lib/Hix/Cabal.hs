{-# language CPP #-}

module Hix.Cabal where

import Control.Monad.Trans.Except (ExceptT, throwE)
import Distribution.PackageDescription (BuildInfo (..), GenericPackageDescription (..))
import Distribution.Types.Benchmark (benchmarkBuildInfo)
import Distribution.Types.CondTree (CondTree (..))
import qualified Distribution.Types.Executable as Executable
import Distribution.Types.Library (Library (..))
import Distribution.Types.TestSuite (testBuildInfo)
import Distribution.Utils.Path (getSymbolicPath)
import qualified Distribution.Verbosity as Cabal
import Exon (exon)
import Path (
  Abs,
  Dir,
  File,
  Path,
  Rel,
  absdir,
  isProperPrefixOf,
  parent,
  parseRelDir,
  parseRelFile,
  stripProperPrefix,
  toFilePath,
  (</>),
  )
import System.FilePattern.Directory (getDirectoryFiles)

import Hix.Compat (readGenericPackageDescription)
import Hix.Data.Error (Error (..), ErrorMessage (Client, Fatal))
import Hix.Data.LogLevel (LogLevel (LogVerbose))
import Hix.Error (pathText, sourceError, throwMessage, tryIO)

#if MIN_VERSION_Cabal(3,14,0)
import Distribution.Utils.Path (makeSymbolicPath)
#endif

noMatch :: Text -> Path b File -> ExceptT Error IO a
noMatch reason source =
  throwE (Error {message = Client (sourceError reason source), context = [], level = Just LogVerbose})

cabalsInDir ::
  Path Abs Dir ->
  ExceptT Error IO [Path Abs File]
cabalsInDir dir = do
  matches <- liftIO (getDirectoryFiles (toFilePath dir) ["*.cabal"])
  let err = Fatal [exon|Internal error when parsing globbed paths in '#{pathText dir}': #{show matches}|]
  maybe (throwMessage err) pure (traverse parse matches)
  where
    parse f = do
      rel <- parseRelFile f
      pure (dir </> rel)

findCabal ::
  Path Abs File ->
  ExceptT Error IO (Path Abs File, Path Rel File)
findCabal source =
  spin (parent source)
  where
    spin dir
      | dir == [absdir|/nix/store|] = notFound
      | dir == parent dir = notFound
      | otherwise = tryDir dir
    tryDir dir =
      cabalsInDir dir >>= \case
        [cabal] -> do
          sub <- stripProperPrefix (parent cabal) source
          pure (cabal, sub)
        [] -> spin (parent dir)
        _ -> throwMessage (Client (sourceError "Multiple cabal files in parent dir of" source))
    notFound =
      noMatch "No cabal file found for " source

parseCabal :: Path Abs File -> ExceptT Error IO GenericPackageDescription
parseCabal path =
  tryIO do
#if MIN_VERSION_Cabal(3,14,0)
    readGenericPackageDescription Cabal.verbose Nothing (makeSymbolicPath (toFilePath path))
#else
    readGenericPackageDescription Cabal.verbose (toFilePath path)
#endif

buildInfo ::
  (a -> BuildInfo) ->
  (b, CondTree c d a) ->
  BuildInfo
buildInfo f (_, t) =
  f t.condTreeData

matchComponent :: GenericPackageDescription -> Path Rel File -> ExceptT Error IO BuildInfo
matchComponent pkg source =
  maybe (noMatch "cabal component" source) pure (find matchSource infos)
  where
    matchSource BuildInfo {..} =
      any (matchSourceDir . getSymbolicPath) hsSourceDirs
    matchSourceDir dir
      | Just p <- parseRelDir dir, isProperPrefixOf p source = True
      | otherwise = False
    infos =
      ((.condTreeData.libBuildInfo) <$> maybeToList pkg.condLibrary) <>
      (buildInfo libBuildInfo <$> pkg.condSubLibraries) <>
      (buildInfo Executable.buildInfo <$> pkg.condExecutables) <>
      (buildInfo testBuildInfo <$> pkg.condTestSuites) <>
      (buildInfo benchmarkBuildInfo <$> pkg.condBenchmarks)

buildInfoForFile :: Path Abs File -> ExceptT Error IO BuildInfo
buildInfoForFile source = do
  (cabalPath, sourceRel) <- findCabal source
  pkg <- parseCabal cabalPath
  matchComponent pkg sourceRel
