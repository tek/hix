{-# language OverloadedRecordDot #-}

module Hix where

import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT, throwE)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Distribution.PackageDescription (BuildInfo (..), GenericPackageDescription (..))
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.Types.Benchmark (benchmarkBuildInfo)
import Distribution.Types.CondTree (CondTree (..))
import qualified Distribution.Types.Executable as Executable
import Distribution.Types.Library (Library (..))
import Distribution.Types.TestSuite (testBuildInfo)
import Distribution.Utils.Path (getSymbolicPath)
import qualified Distribution.Verbosity as Cabal
import Exon (exon)
import Hix.Options (Command (Preproc), GlobalOptions (..), Options (Options), PreprocOptions (..), parseCli)
import Language.Haskell.Extension (Extension (DisableExtension, EnableExtension, UnknownExtension))
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
import System.IO (stderr)
import System.IO.Error (tryIOError)

pathText :: Path b t -> Text
pathText =
  toText . toFilePath

data Error =
  PreprocError Text
  |
  NoMatch Text
  deriving stock (Eq, Show, Generic)

printPreprocError ::
  MonadIO m =>
  Text ->
  m ()
printPreprocError msg =
  liftIO (Text.hPutStrLn stderr [exon|>>> Preprocessor generator failed: #{msg}|])

sourceError :: Text -> Path b t -> Text
sourceError reason source =
  [exon|#{reason} the source file '#{pathText source}'|]

noMatch :: Text -> Path b File -> ExceptT Error IO a
noMatch reason source =
  throwE (NoMatch (sourceError reason source))

handleError ::
  MonadIO m =>
  GlobalOptions ->
  Error ->
  m ()
handleError GlobalOptions {verbose} = \case
  PreprocError err -> printPreprocError err
  NoMatch msg | (fromMaybe False verbose) -> printPreprocError msg
  NoMatch _ -> unit

cabalsInDir ::
  Path Abs Dir ->
  ExceptT Error IO [Path Abs File]
cabalsInDir dir = do
  matches <- liftIO (getDirectoryFiles (toFilePath dir) ["*.cabal"])
  let err = PreprocError [exon|Internal error when parsing globbed paths in '#{pathText dir}': #{show matches}|]
  maybe (throwE err) pure (traverse parse matches)
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
        _ -> throwE (PreprocError (sourceError "Multiple cabal files in parent dir of" source))
    notFound =
      noMatch "No cabal file found for " source

parseCabal :: Path Abs File -> ExceptT Error IO GenericPackageDescription
parseCabal path =
  ExceptT $ fmap (first (PreprocError . show)) $ tryIOError do
    readGenericPackageDescription Cabal.verbose (toFilePath path)

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

extension :: Extension -> Maybe Text
extension = \case
  EnableExtension ext -> Just (show ext)
  DisableExtension ext -> Just ("No" <> show ext)
  UnknownExtension _ -> Nothing

extensionsPragma :: BuildInfo -> [Text]
extensionsPragma info
  | null exts =
    []
  | otherwise =
    [[exon|{-# language #{Text.intercalate ", " exts} #-}|]]
  where
    exts = mapMaybe extension info.defaultExtensions

-- TODO add common stanzas
runCommand :: Command -> ExceptT Error IO ()
runCommand (Preproc PreprocOptions {..}) = do
  (cabalPath, sourceRel) <- findCabal source
  pkg <- parseCabal cabalPath
  info <- matchComponent pkg sourceRel
  for_ (extensionsPragma info) \ ln -> liftIO (Text.putStrLn ln)

main :: IO ()
main = do
  Options global cmd <- parseCli
  leftA (handleError global) =<< runExceptT (runCommand cmd)
