module Hix.Ghci where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, catchE)
import Data.List.Extra (nubOrd)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Exon (exon)
import Path (Abs, Dir, File, Path, Rel, parseRelDir, reldir, splitExtension, stripProperPrefix, toFilePath, (</>))
import Path.IO (createDirIfMissing, getTempDir, openTempFile)
import System.IO (hClose)
import System.Posix.User (getLoginName)

import Hix.Component (targetComponentOrError)
import qualified Hix.Data.ComponentConfig
import Hix.Data.ComponentConfig (
  ComponentConfig,
  ModuleName (ModuleName),
  PackageConfig,
  PackageName,
  SourceDir (SourceDir),
  Target (Target),
  )
import Hix.Data.Error (Error (GhciError), note, pathText, tryIO)
import qualified Hix.Data.GhciConfig
import Hix.Data.GhciConfig (GhciConfig, GhciRunExpr (GhciRunExpr), GhciSetupCode (GhciSetupCode))
import qualified Hix.Data.GhciTest as GhciTest
import Hix.Data.GhciTest (GhciRun (GhciRun), GhciTest (GhciTest), GhcidRun (GhcidRun))
import Hix.Json (jsonConfigE)
import Hix.Monad (M, noteGhci)
import qualified Hix.Options as Options
import Hix.Options (
  ExtraGhciOptions (ExtraGhciOptions),
  ExtraGhcidOptions (ExtraGhcidOptions),
  GhciOptions (GhciOptions),
  GhcidOptions,
  TargetSpec (TargetForFile),
  TestOptions (TestOptions),
  )
import Hix.Path (rootDir)

relativeToComponent ::
  Path Abs Dir ->
  PackageConfig ->
  Maybe SourceDir ->
  Path Abs File ->
  M (Path Rel File)
relativeToComponent root package mdir path = do
  SourceDir dir <- noteGhci "Internal: No source dir for file target" mdir
  noteGhci "Internal: Bad file target" (stripProperPrefix (root </> package.src </> dir) path)

moduleName ::
  PackageConfig ->
  Maybe SourceDir ->
  GhciOptions ->
  M ModuleName
moduleName package component = \case
  GhciOptions {component = TargetForFile path, root = cliRoot} -> do
    root <- rootDir cliRoot
    rel <- relativeToComponent root package component path
    pure (ModuleName (Text.replace "/" "." (withoutExt rel)))
  GhciOptions {test} -> pure test.mod
  where
    withoutExt p = pathText (maybe p fst (splitExtension p))

ghciScript ::
  GhciConfig ->
  PackageConfig ->
  Maybe SourceDir ->
  GhciOptions ->
  M Text
ghciScript config package component opt = do
  ModuleName module_ <- moduleName package component opt
  pure [exon|#{cdCode}#{setup}
:load #{module_}
import #{module_}|]
  where
    cdCode | opt.test.cd.unChangeDir = [exon|:cd #{pathText package.src}
|]
           | otherwise = ""
    GhciSetupCode setup = fold (flip Map.lookup config.setup =<< opt.test.runner)

componentSearchPaths :: PackageConfig -> ComponentConfig -> [Path Rel Dir]
componentSearchPaths pkg comp = do
  SourceDir dir <- coerce comp.sourceDirs
  pure (pkg.src </> dir)

librarySearchPaths :: Map PackageName PackageConfig -> [Path Rel Dir]
librarySearchPaths pkgs = do
  pkg <- Map.elems pkgs
  comp <- maybeToList (pkg.components !? "library")
  componentSearchPaths pkg comp

searchPath :: Map PackageName PackageConfig -> PackageConfig -> ComponentConfig -> [Path Rel Dir]
searchPath pkgs pkg comp =
  nubOrd (componentSearchPaths pkg comp <> librarySearchPaths pkgs)

testRun :: GhciConfig -> TestOptions -> Maybe Text
testRun config = \case
  TestOptions {test, runner = Just runner} | Just (GhciRunExpr run) <- config.run !? runner ->
    Just [exon|(#{run}) #{fold test}|]
  TestOptions {test = Just test} ->
    Just test
  TestOptions {test = Nothing} ->
    Nothing

assemble :: GhciOptions -> M GhciTest
assemble opt = do
  config <- jsonConfigE GhciError opt.config
  root <- rootDir opt.root
  Target {..} <- targetComponentOrError opt.root config.mainPackage config.packages opt.component
  script <- ghciScript config package sourceDir opt
  pure GhciTest {
    script,
    test = testRun config opt.test,
    args = config.args,
    searchPath = (root </>) <$> searchPath config.packages package component
    }

hixTempDir ::
  ExceptT Error IO (Path Abs Dir)
hixTempDir = do
  tmp <- tryIO getTempDir
  user <- note "Couldn't determine user name" . parseRelDir =<< catchE (tryIO getLoginName) (const (pure "user"))
  let hixTmp = tmp </> [reldir|hix|] </> user
  tryIO (createDirIfMissing True hixTmp)
  pure hixTmp

ghciScriptFile ::
  Path Abs Dir ->
  Text ->
  ExceptT Error IO (Path Abs File)
ghciScriptFile tmp text =
  tryIO do
    (path, handle) <- openTempFile tmp "hix-ghci-.ghci"
    Text.hPutStr handle text
    hClose handle
    pure path

argFrag :: Text -> Text
argFrag "" = ""
argFrag s = [exon| #{s}|]

optArg :: Maybe Text -> Text
optArg = foldMap argFrag

searchPathArg :: NonEmpty (Path Abs Dir) -> Text
searchPathArg paths =
  [exon|-i#{colonSeparated}|]
  where
    colonSeparated = Text.intercalate ":" (pathText <$> toList paths)

ghciCmdline ::
  GhciTest ->
  Maybe ExtraGhciOptions ->
  Path Abs File ->
  Maybe (Path Abs File) ->
  GhciRun
ghciCmdline test extra scriptFile runScriptFile =
  GhciRun {..}
  where
    cmdline = [exon|ghci#{shell}#{optArg run}|]
    shell = [exon|#{argFrag args}#{argFrag sp} -ghci-script=##{toFilePath scriptFile}#{argFrag extraOpts}|]
    args = Text.unwords (coerce test.args)
    run = runScriptFile <&> \ f -> [exon|-ghci-script=##{toFilePath f}|]
    sp = foldMap searchPathArg (nonEmpty test.searchPath)
    extraOpts | Just (ExtraGhciOptions o) <- extra = o
              | otherwise = ""

ghciCmdlineFromOptions ::
  Path Abs Dir ->
  GhciOptions ->
  M GhciRun
ghciCmdlineFromOptions tmp opt = do
  conf <- assemble opt
  shellScriptFile <- lift (ghciScriptFile tmp conf.script)
  runScriptFile <- lift (traverse (ghciScriptFile tmp) conf.test)
  pure (ghciCmdline conf opt.extra shellScriptFile runScriptFile)

ghcidCmdlineFromOptions ::
  Path Abs Dir ->
  GhcidOptions ->
  M GhcidRun
ghcidCmdlineFromOptions tmp opt = do
  ghci <- ghciCmdlineFromOptions tmp opt.ghci
  let
    test = fromMaybe "main" ghci.test.test
  pure (GhcidRun [exon|ghcid --command="ghci#{ghci.shell}" --test='##{test}'#{foldMap extra opt.extra}|] ghci)
  where
    extra (ExtraGhcidOptions o) = [exon| ##{o}|]

printGhciCmdline ::
  GhciOptions ->
  M ()
printGhciCmdline opt = do
  tmp <- lift hixTempDir
  cmd <- ghciCmdlineFromOptions tmp opt
  liftIO (Text.putStrLn [exon|ghci #{cmd.shell} #{fold cmd.run}|])

printGhcidCmdline ::
  GhcidOptions ->
  M ()
printGhcidCmdline opt = do
  tmp <- lift hixTempDir
  cmd <- ghcidCmdlineFromOptions tmp opt
  liftIO (Text.putStrLn cmd.cmdline)
