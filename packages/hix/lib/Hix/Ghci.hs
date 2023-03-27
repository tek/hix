module Hix.Ghci where

import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.List.Extra (firstJust, nubOrd)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Exon (exon)
import Hix.Data.Error (Error (GhcidError), pathText)
import qualified Hix.Data.GhciConfig as GhciConfig
import Hix.Data.GhciConfig (
  ComponentConfig,
  GhciRunExpr (GhciRunExpr),
  GhciSetupCode (GhciSetupCode),
  PackageConfig (PackageConfig),
  PackageName,
  SourceDir (SourceDir),
  )
import qualified Hix.Data.GhcidTest as GhcidTest
import Hix.Data.GhcidTest (GhciRun (GhciRun), GhcidRun (GhcidRun), GhcidTest (GhcidTest))
import qualified Hix.Options as Options
import Hix.Options (GhciOptions (GhciOptions), GhcidFileSpec (GhcidFileSpec), GhcidModuleSpec, runner)
import Path (
  Abs,
  Dir,
  File,
  Path,
  Rel,
  filename,
  isProperPrefixOf,
  parseRelDir,
  reldir,
  splitExtension,
  stripProperPrefix,
  toFilePath,
  (</>),
  )
import Path.IO (createDirIfMissing, getCurrentDir, getTempDir, openTempFile)
import System.IO (hClose)
import System.Posix.User (getLoginName)

-- TODO IO errors in this module

note :: Text -> Maybe a -> ExceptT Error IO a
note err =
  maybe (throwE (GhcidError err)) pure

targetForModule :: GhciOptions -> GhcidModuleSpec -> ExceptT Error IO (PackageConfig, ComponentConfig)
targetForModule opts spec = do
  pkg <- note "No such package" (opts.config.packages !? spec.package)
  comp <- note "No component for source dir" (find matchSourceDir (Map.elems pkg.components))
  pure (pkg, comp)
  where
    matchSourceDir comp = elem @[] spec.sourceDir (coerce comp.sourceDirs)

targetForFile :: GhciOptions -> GhcidFileSpec -> ExceptT Error IO (PackageConfig, ComponentConfig)
targetForFile opts (GhcidFileSpec file) = do
  cwd <- getCurrentDir
  fileRel <- stripProperPrefix cwd file
  (pkg, subpath) <- note "No package contains this file" (firstJust (matchPackage fileRel) (Map.elems opts.config.packages))
  comp <- note "No component source dir contains this file" (find (matchSourceDir subpath) (Map.elems pkg.components))
  pure (pkg, comp)
  where
    matchPackage fileRel pkg@PackageConfig {src} = do
      subpath <- stripProperPrefix src fileRel
      pure (pkg, subpath)
    matchSourceDir subpath comp = any @[] (\ (SourceDir dir) -> isProperPrefixOf dir subpath) (coerce comp.sourceDirs)

targetComponent :: GhciOptions -> ExceptT Error IO (PackageConfig, ComponentConfig)
targetComponent = \case
  opts@GhciOptions {spec = Right spec} ->
    targetForModule opts spec
  opts@GhciOptions {spec = Left spec} ->
    targetForFile opts spec

moduleName :: GhciOptions -> Text
moduleName = \case
  GhciOptions {spec = Right spec} -> spec.module_
  GhciOptions {spec = Left (GhcidFileSpec path)} -> withoutExt (filename path)
  where
    withoutExt p = pathText (maybe p fst (splitExtension p))

ghciScript :: GhciOptions -> Text
ghciScript opt =
  [exon|#{setup}
:load #{module_}
import #{module_}|]
  where
    module_ = moduleName opt
    GhciSetupCode setup = fold (opt.config.setup !? opt.runner)

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

testRun :: GhciOptions -> Text -> Text
testRun opt test =
  [exon|#{run}#{test}|]
  where
    run | Just (GhciRunExpr r) <- opt.config.run !? opt.runner = [exon|(#{r}) |]
        | otherwise = ""

assemble :: GhciOptions -> ExceptT Error IO GhcidTest
assemble opt = do
  cwd <- getCurrentDir
  (pkg, comp) <- targetComponent opt
  pure GhcidTest {
    script = ghciScript opt,
    test = testRun opt <$> opt.test,
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
  Maybe (Path Abs File )->
  GhciRun
ghciCmdline test scriptFile runScriptFile = do
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

ghcidEnv :: GhciOptions -> ExceptT Error IO (Path Abs File)
ghcidEnv opts = do
  (_, target) <- targetComponent opts
  pure target.runner

printGhciEnv :: GhciOptions -> ExceptT Error IO ()
printGhciEnv opts = do
  runner <- ghcidEnv opts
  liftIO (Text.putStrLn (pathText runner))
