module Hix.Ghci where

import Control.Monad.Trans.Except (ExceptT, catchE)
import Data.List.Extra (nubOrd)
import Data.List.NonEmpty (appendList, prependList)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Distribution.Simple (Dependency (..))
import Exon (exon)
import Path (Abs, Dir, File, Path, Rel, parseRelDir, reldir, splitExtension, stripProperPrefix, toFilePath, (</>))
import Path.IO (createDirIfMissing, getTempDir, openTempFile)
import System.IO (hClose)
import System.Posix.User (getLoginName)

import Hix.Component (targetComponentOrError)
import qualified Hix.Data.ComponentConfig
import Hix.Data.ComponentConfig (
  ComponentConfig,
  ComponentDep (..),
  ModuleName (..),
  PackageConfig (..),
  SourceDir (..),
  Target (..),
  )
import qualified Hix.Data.ComponentName as ComponentName
import qualified Hix.Data.GhciConfig
import Hix.Data.GhciConfig (
  CommandEnvContext,
  GhciArgs (..),
  GhciContext,
  GhciRunExpr (GhciRunExpr),
  GhciSetupCode (GhciSetupCode),
  )
import qualified Hix.Data.GhciTest as GhciTest
import Hix.Data.GhciTest (GhciRun (GhciRun), GhciTest (GhciTest), GhcidRun (GhcidRun))
import Hix.Data.Monad (liftE)
import qualified Hix.Data.Options as Options
import Hix.Data.Options (CommandOptions (..), GhciOptions (..), GhcidOptions, TargetSpec (..), TestOptions (..))
import qualified Hix.Data.PackageName as PackageName
import Hix.Data.PackageName (PackageName)
import Hix.Env (commandEnv, runEnvProcess)
import Hix.Error (Error, ErrorMessage (..), pathText, throwMessage, tryIO)
import Hix.Json (resolveContext)
import qualified Hix.Managed.Handlers.Context as Context
import Hix.Managed.Handlers.Context (ContextHandlers)
import Hix.Maybe (fromMaybeA)
import Hix.Monad (M, noteGhci, withTempDir)
import Hix.Path (PathSpecResolver (resolvePathSpec), rootDir)

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
  GhciOptions {command = CommandOptions {component = Just (TargetForFile path), root = cliRoot}} -> do
    root <- rootDir =<< traverse resolvePathSpec cliRoot
    rel <- relativeToComponent root package component =<< resolvePathSpec path
    pure (ModuleName (Text.replace "/" "." (withoutExt rel)))
  GhciOptions {test} -> pure test.mod
  where
    withoutExt p = pathText (maybe p fst (splitExtension p))

ghciScript ::
  GhciContext ->
  PackageConfig ->
  Maybe SourceDir ->
  GhciOptions ->
  M Text
ghciScript config package component options = do
  ModuleName module_ <- moduleName package component options
  pure [exon|#{cdCode}#{setup}
:load #{module_}
import #{module_}|]
  where
    cdCode | options.test.cd.unChangeDir = [exon|:cd #{pathText package.src}
|]
           | otherwise = ""
    GhciSetupCode setup = fold (flip Map.lookup config.setup =<< options.test.runner)

componentSearchPaths :: Path Rel Dir -> ComponentConfig -> [Path Rel Dir]
componentSearchPaths src comp = do
  SourceDir dir <- coerce comp.sourceDirs
  pure (src </> dir)

depClosure ::
  Map PackageName PackageConfig ->
  ComponentConfig ->
  [(Path Rel Dir, ComponentConfig)]
depClosure pkgs =
  Map.elems . spin []
  where
    spin z comp =
      foldl' dep z comp.deps

    dep z (ComponentDep (Dependency pkgName _ comps)) =
      case pkgs !? PackageName.fromCabal pkgName of
        Just pkg -> foldl' (depComp pkg) z comps
        Nothing -> z

    depComp PackageConfig {name = pkgName, src, components} z (ComponentName.fromCabal -> compName)
      | let key = (pkgName, compName)
      , not (Map.member key z)
      , Just comp <- components !? compName
      = spin (Map.insert key (src, comp) z) comp

      | otherwise
      = z

depSearchPath :: Map PackageName PackageConfig -> PackageConfig -> ComponentConfig -> [Path Rel Dir]
depSearchPath pkgs pkg comp =
  nubOrd (concatMap (uncurry componentSearchPaths) components)
  where
    components = (pkg.src, comp) : depClosure pkgs comp

librarySearchPaths :: Map PackageName PackageConfig -> [Path Rel Dir]
librarySearchPaths pkgs = do
  pkg <- Map.elems pkgs
  comp <- maybeToList (pkg.components !? "library")
  componentSearchPaths pkg.src comp

legacySearchPath :: Map PackageName PackageConfig -> PackageConfig -> ComponentConfig -> [Path Rel Dir]
legacySearchPath pkgs pkg comp =
  nubOrd (componentSearchPaths pkg.src comp <> librarySearchPaths pkgs)

testRun :: GhciContext -> TestOptions -> Maybe Text
testRun config = \case
  TestOptions {test, runner = Just runner} | Just (GhciRunExpr run) <- config.run !? runner ->
    Just [exon|(#{run}) #{fold test}|]
  TestOptions {test = Just test} ->
    Just test
  TestOptions {test = Nothing} ->
    Nothing

assemble :: GhciOptions -> GhciContext -> M GhciTest
assemble options context = do
  mRoot <- traverse resolvePathSpec options.command.root
  root <- rootDir mRoot
  Target {..} <- targetComponentOrError mRoot context.command.mainPackage context.command.packages options.command.component
  script <- ghciScript context package sourceDir options
  let searchPath = if context.manualCabal then legacySearchPath else depSearchPath
  pure GhciTest {
    script,
    test = testRun context options.test,
    args = context.args,
    searchPath = (root </>) <$> searchPath context.command.packages package component
  }

hixTempDir :: ExceptT Error IO (Path Abs Dir)
hixTempDir = do
  tmp <- tryIO getTempDir
  user <- parseError . parseRelDir =<< catchE (tryIO getLoginName) (const (pure "user"))
  let hixTmp = tmp </> [reldir|hix|] </> user
  tryIO (createDirIfMissing True hixTmp)
  pure hixTmp
  where
    parseError = fromMaybeA (throwMessage (FatalExternal "Couldn't convert user name to path"))

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
  GhciArgs ->
  [Text] ->
  Path Abs File ->
  Maybe (Path Abs File) ->
  GhciRun
ghciCmdline test extra args scriptFile runScriptFile =
  GhciRun {..}
  where
    shell = appendList (prependList (coerce test.args ++ searchPath) [scriptArg]) extraOpts

    scriptArg = [exon|-ghci-script=##{toFilePath scriptFile}|]

    run = runScriptFile <&> \ f -> [exon|-ghci-script=##{toFilePath f}|]

    searchPath = foldMap (pure . searchPathArg) (nonEmpty test.searchPath)

    extraOpts = extra.text ++ args

ghciCmdlineFromContext ::
  Path Abs Dir ->
  GhciOptions ->
  GhciContext ->
  CommandEnvContext ->
  M GhciRun
ghciCmdlineFromContext tmp options context envContext = do
  conf <- assemble options context
  shellScriptFile <- liftE (ghciScriptFile tmp conf.script)
  runScriptFile <- liftE (traverse (ghciScriptFile tmp) conf.test)
  pure (ghciCmdline conf (envContext.ghciArgs <> options.extra) options.args shellScriptFile runScriptFile)

ghciCmdlineFromOptions ::
  ContextHandlers ->
  Path Abs Dir ->
  GhciOptions ->
  M GhciRun
ghciCmdlineFromOptions contextHandlers tmp options = do
  (context, envContext) <- ghciContexts contextHandlers options
  ghciCmdlineFromContext tmp options context envContext

ghcidCmdlineFromContext ::
  Path Abs Dir ->
  GhcidOptions ->
  GhciContext ->
  CommandEnvContext ->
  M GhcidRun
ghcidCmdlineFromContext tmp options context envContext = do
  ghci <- ghciCmdlineFromContext tmp options.ghci context envContext
  let test = fromMaybe "main" ghci.test.test
  pure GhcidRun {
    args = appendList [
      [exon|--command=ghci #{Text.unwords (toList ghci.shell)}|],
      [exon|--test=##{test}|]
    ] (envContext.ghcidArgs <> options.extra).text,
    ghci
  }

ghcidCmdlineFromOptions ::
  ContextHandlers ->
  Path Abs Dir ->
  GhcidOptions ->
  M GhcidRun
ghcidCmdlineFromOptions contextHandlers tmp options = do
  (context, envContext) <- ghciContexts contextHandlers options.ghci
  ghci <- ghciCmdlineFromContext tmp options.ghci context envContext
  let test = fromMaybe "main" ghci.test.test
  pure GhcidRun {
    args = appendList [
      [exon|--command=ghci #{Text.unwords (toList ghci.shell)}|],
      [exon|--test=##{test}|]
    ] (envContext.ghcidArgs <> options.extra).text,
    ghci
  }

quoteArgs ::
  Functor t =>
  t Text ->
  t Text
quoteArgs =
  fmap \ a -> [exon|"#{Text.replace "\"" "\\\"" a}"|]

ghciEnvContext ::
  ContextHandlers ->
  GhciOptions ->
  M CommandEnvContext
ghciEnvContext contextHandlers options = do
  context <- resolveContext options.context
  commandEnv contextHandlers context.command options.command

ghciContexts ::
  ContextHandlers ->
  GhciOptions ->
  M (GhciContext, CommandEnvContext)
ghciContexts contextHandlers options = do
  context <- resolveContext options.context
  envContext <- ghciEnvContext contextHandlers options
  pure (context, envContext)

printGhciCmdline ::
  GhciOptions ->
  M ()
printGhciCmdline options = do
  tmp <- liftE hixTempDir
  cmd <- ghciCmdlineFromOptions Context.handlersProd tmp options
  let cmdline = "ghci" : quoteArgs (toList cmd.shell ++ maybeToList cmd.run)
  liftIO (Text.putStrLn (Text.unwords cmdline))

printGhcidCmdline ::
  GhcidOptions ->
  M ()
printGhcidCmdline options = do
  tmp <- liftE hixTempDir
  cmd <- ghcidCmdlineFromOptions Context.handlersProd tmp options
  let cmdline = "ghcid" : toList (quoteArgs cmd.args)
  liftIO (Text.putStrLn (Text.unwords cmdline))

argsGhciRun :: GhciRun -> [Text]
argsGhciRun cmd =
  toList cmd.shell ++ maybeToList cmd.run

runGhci :: GhciOptions -> M ()
runGhci options =
  withTempDir "ghci-cmd" \ tmp -> do
    (context, envContext) <- ghciContexts Context.handlersProd options
    cmd <- ghciCmdlineFromContext tmp options context envContext
    runEnvProcess envContext "ghci" (argsGhciRun cmd)

runGhcid :: GhcidOptions -> M ()
runGhcid options =
  withTempDir "ghcid-cmd" \ tmp -> do
    (context, envContext) <- ghciContexts Context.handlersProd options.ghci
    cmd <- ghcidCmdlineFromContext tmp options context envContext
    runEnvProcess envContext "ghcid" (toList cmd.args)
