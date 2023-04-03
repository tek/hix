module Hix.Ghci where

import Control.Monad.Trans.Except (ExceptT)
import Data.List.Extra (nubOrd)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Exon (exon)
import Path (Abs, Dir, File, Path, Rel, filename, parseRelDir, reldir, splitExtension, toFilePath, (</>))
import Path.IO (createDirIfMissing, getCurrentDir, getTempDir, openTempFile)
import System.IO (hClose)
import System.Posix.User (getLoginName)

import Hix.Component (targetComponent)
import Hix.Data.Error (Error, pathText)
import qualified Hix.Data.GhciConfig as GhciConfig
import Hix.Data.GhciConfig (
  ComponentConfig,
  GhciConfig,
  GhciRunExpr (GhciRunExpr),
  GhciSetupCode (GhciSetupCode),
  ModuleName (ModuleName),
  PackageConfig,
  PackageName,
  SourceDir (SourceDir),
  )
import qualified Hix.Data.GhcidTest as GhcidTest
import Hix.Data.GhcidTest (GhciRun (GhciRun), GhcidRun (GhcidRun), GhcidTest (GhcidTest))
import qualified Hix.Options as Options
import Hix.Options (
  ComponentSpec (ComponentForFile, ComponentForModule),
  GhciOptions (GhciOptions),
  TestOptions (TestOptions),
  )

-- TODO IO errors in this module

-- TODO this doesn't work for multi-segment module names
moduleName :: GhciOptions -> ModuleName
moduleName = \case
  GhciOptions {component = ComponentForModule spec} -> spec.mod
  GhciOptions {component = ComponentForFile path} -> ModuleName (withoutExt (filename path))
  where
    withoutExt p = pathText (maybe p fst (splitExtension p))

ghciScript :: GhciOptions -> Text
ghciScript opt =
  [exon|#{setup}
:load #{module_}
import #{module_}|]
  where
    ModuleName module_ = moduleName opt
    GhciSetupCode setup = fold (flip Map.lookup opt.config.setup =<< opt.test.runner)

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
  TestOptions test (Just runner) | Just (GhciRunExpr run) <- config.run !? runner ->
    Just [exon|(#{run}) #{fold test}|]
  TestOptions (Just test) _ ->
    Just test
  TestOptions Nothing _ ->
    Nothing

assemble :: GhciOptions -> ExceptT Error IO GhcidTest
assemble opt = do
  cwd <- getCurrentDir
  (pkg, comp) <- targetComponent opt.config.packages opt.component
  pure GhcidTest {
    script = ghciScript opt,
    test = testRun opt.config opt.test,
    args = opt.config.args,
    searchPath = (cwd </>) <$> searchPath opt.config.packages pkg comp
    }

hixTempDir ::
  ExceptT Error IO (Path Abs Dir)
hixTempDir = do
  tmp <- getTempDir
  user <- parseRelDir =<< liftIO getLoginName
  let hixTmp = tmp </> [reldir|hix|] </> user
  createDirIfMissing True hixTmp
  pure hixTmp

ghciScriptFile ::
  Path Abs Dir ->
  Text ->
  ExceptT Error IO (Path Abs File)
ghciScriptFile tmp text = do
  (path, handle) <- openTempFile tmp "hix-ghci-.ghci"
  liftIO (Text.hPutStr handle text)
  liftIO (hClose handle)
  pure path

searchPathArg :: NonEmpty (Path Abs Dir) -> Text
searchPathArg paths =
  [exon|-i#{colonSeparated}|]
  where
    colonSeparated = Text.intercalate ":" (pathText <$> toList paths)

ghciCmdline ::
  GhcidTest ->
  Path Abs File ->
  Maybe (Path Abs File) ->
  GhciRun
ghciCmdline test scriptFile runScriptFile =
  GhciRun {..}
  where
    shell = [exon|##{Text.unwords (coerce test.args)} #{sp} -ghci-script=##{toFilePath scriptFile}|]
    run = runScriptFile <&> \ f -> [exon|-ghci-script=##{toFilePath f}|]
    sp = foldMap searchPathArg (nonEmpty test.searchPath)

ghciCmdlineFromOptions ::
  Path Abs Dir ->
  GhciOptions ->
  ExceptT Error IO GhciRun
ghciCmdlineFromOptions tmp opt = do
  conf <- assemble opt
  shellScriptFile <- ghciScriptFile tmp conf.script
  runScriptFile <- traverse (ghciScriptFile tmp) conf.test
  pure (ghciCmdline conf shellScriptFile runScriptFile)

ghcidCmdlineFromOptions ::
  Path Abs Dir ->
  GhciOptions ->
  ExceptT Error IO GhcidRun
ghcidCmdlineFromOptions tmp opt = do
  ghci <- ghciCmdlineFromOptions tmp opt
  pure (GhcidRun [exon|ghcid --command="ghci #{ghci.shell}" --test='##{fromMaybe "main" ghci.test.test}'|] ghci)

printGhciCmdline ::
  GhciOptions ->
  ExceptT Error IO ()
printGhciCmdline opt = do
  tmp <- hixTempDir
  cmd <- ghciCmdlineFromOptions tmp opt
  liftIO (Text.putStrLn [exon|ghci #{cmd.shell} #{fold cmd.run}|])

printGhcidCmdline ::
  GhciOptions ->
  ExceptT Error IO ()
printGhcidCmdline opt = do
  tmp <- hixTempDir
  cmd <- ghcidCmdlineFromOptions tmp opt
  liftIO (Text.putStrLn cmd.cmdline)
