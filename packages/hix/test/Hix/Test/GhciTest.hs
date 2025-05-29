module Hix.Test.GhciTest where

import Control.Monad.Trans.Class (lift)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Distribution.Simple (Dependency)
import Exon (exon)
import Hedgehog (evalEither, (===))
import Path (Abs, Dir, File, Path, Rel, SomeBase (Abs, Rel), absdir, absfile, reldir, relfile, (</>))
import Path.IO (withSystemTempDir)
import Test.Tasty (TestTree, testGroup)

import Hix.Data.ComponentConfig (
  ComponentConfig (..),
  ComponentDep (..),
  ComponentName,
  EnvRunner (EnvRunner),
  PackageConfig (..),
  PackagesConfig,
  SourceDir (SourceDir),
  SourceDirs (SourceDirs),
  )
import Hix.Data.EnvName (EnvName)
import Hix.Data.GhciConfig (ChangeDir (ChangeDir), CommandContext (..), CommandEnvContext (..), GhciContext (..))
import qualified Hix.Data.GhciTest as GhciTest
import qualified Hix.Data.Options as Options
import qualified Hix.Data.Options as GhciOptions (GhciOptions (..))
import Hix.Data.Options (
  CommandOptions (..),
  ComponentCoords (ComponentCoords),
  ComponentSpec (ComponentSpec),
  GhciOptions (..),
  GhcidOptions (GhcidOptions),
  PackageSpec (PackageSpec),
  TargetSpec (TargetForComponent, TargetForFile),
  TestOptions (TestOptions),
  )
import Hix.Data.PathSpec (PathSpec (PathConcrete))
import Hix.Env (commandEnv)
import Hix.Error (pathText)
import Hix.Ghci (argsGhciRun, assemble, ghciCmdlineFromOptions, ghcidCmdlineFromOptions)
import qualified Hix.Managed.Handlers.Context as Context
import Hix.Managed.Handlers.Context (ContextHandlers, ContextKey (..), ContextQuery (..))
import Hix.Monad (runM)
import Hix.Test.Utils (UnitTest, unitTest)

root :: Path Abs Dir
root =
  [absdir|/project|]

runner1 :: EnvRunner
runner1 =
  EnvRunner [absfile|/runner1.bash|]

runner2 :: EnvRunner
runner2 =
  EnvRunner [absfile|/runner2.bash|]

defaultRunner :: EnvRunner
defaultRunner =
  EnvRunner [absfile|/default|]

component :: ComponentName -> Path Rel Dir -> Set Dependency -> EnvName -> ComponentConfig
component name dir deps env =
  ComponentConfig {
    name,
    sourceDirs = SourceDirs [SourceDir dir],
    env = Just env,
    extensions = [],
    language = "GHC2021",
    ghcOptions = [],
    prelude = Nothing,
    deps = Set.map ComponentDep deps
  }

packages :: PackagesConfig
packages =
  [
    ("api", PackageConfig {
      name = "api",
      src = [reldir|packages/api|],
      components = [
        ("library", component "api" [reldir|lib|] ["core:tools"] "env1"),
        ("testing", component "testing" [reldir|testing|] ["api:testing"] "env1"),
        ("server", component "server" [reldir|app|] ["api"] "env1"),
        ("api-test", component "api-test" [reldir|test|] ["api:testing", "api"] "env1")
      ]
    }),
    ("core", PackageConfig {
      name = "core",
      src = [reldir|packages/core|],
      components = [
        ("library", component "core" [reldir|lib|] ["core"] "env1"),
        ("tools", component "tools" [reldir|tools|] ["core"] "env2"),
        ("core-test", component "core-test" [reldir|test|] ["core"] "env2")
      ]
    })
  ]

spec1 :: TargetSpec
spec1 =
  TargetForComponent ComponentCoords {
    package = Just (PackageSpec "api" (Just (Rel [reldir|api|]))),
    component = Just (ComponentSpec "test" (Just (SourceDir [reldir|test|])))
  }

commandContext :: CommandContext
commandContext =
  CommandContext {
    packages,
    defaultEnv = Nothing,
    mainPackage = Nothing
  }

ghciContext :: GhciContext
ghciContext =
  GhciContext {
    command = commandContext,
    setup = [("generic", "import Test.Tasty")],
    run = [("generic", "check . property . test")],
    args = ["-Werror"],
    manualCabal = False
  }

ghciOptions :: GhciOptions
ghciOptions =
  GhciOptions {
    context = Left ghciContext,
    command = CommandOptions {
      root = Nothing,
      component = Just spec1,
      env = Nothing
    },
    test = TestOptions {
      mod = "Api.ServerTest",
      test = Just "test_server",
      runner = Just "generic",
      cd = ChangeDir True
    },
    extra = [],
    args = []
  }

options :: GhcidOptions
options =
  GhcidOptions {ghci = ghciOptions, extra = []}

searchPath :: Text -> [Text] -> Text
searchPath dir subs =
  Text.intercalate ":" [[exon|#{dir}packages/#{sub}/|] | sub <- subs]

ghcidTarget ::
  Path Abs Dir ->
  Path Abs File ->
  [Text]
ghcidTarget cwd scriptFile =
  [
    [exon|--command=ghci -Werror -i#{path} -ghci-script=#{pathText scriptFile}|],
    [exon|--test=#{test}|]
  ]
  where
    test = "(check . property . test) test_server"
    path = searchPath dir ["api/test", "api/lib", "api/testing", "core/lib", "core/tools"]
    dir = pathText cwd

contextHandlers :: ContextHandlers
contextHandlers =
  Context.handlersTest \case
    ContextQuery (ContextCommandEnv name) -> do
      let
        envRunner = \case
          "env1" -> Just runner1
          "env2" -> Just runner2
          _ -> Nothing
      pure $ envRunner name <&> \ runner ->
        CommandEnvContext {ghcidArgs = [], ghciArgs = [], runner}
    _ -> pure Nothing

test_ghcid :: UnitTest
test_ghcid = do
  res <- lift $ withSystemTempDir "hix-test" \ tmp ->
    runM root (ghcidCmdlineFromOptions contextHandlers tmp options)
  cmdline <- evalEither res
  ghcidTarget root cmdline.ghci.scriptFile === toList cmdline.args

mainOptions :: GhciOptions
mainOptions =
  GhciOptions {
    context = Left GhciContext {
      command = CommandContext {
        packages,
        defaultEnv = Nothing,
        mainPackage = Just "core"
      },
      setup = [("generic", "import Test.Tasty")],
      run = [("generic", "")],
      args = [],
      manualCabal = False
    },
    command = CommandOptions {
      root = Nothing,
      component = Just (TargetForComponent (ComponentCoords Nothing Nothing)),
      env = Nothing
    },
    test = TestOptions {
      mod = "Main",
      test = Nothing,
      runner = Nothing,
      cd = ChangeDir True
    },
    extra = [],
    args = []
  }

mainPackageTarget ::
  Path Abs Dir ->
  Path Abs File ->
  [Text]
mainPackageTarget cwd scriptFile =
  [
    [exon|-i#{path}|],
    [exon|-ghci-script=#{pathText scriptFile}|]
  ]
  where
    path = searchPath dir ["core/test", "core/lib"]
    dir = pathText cwd

test_mainPackage :: UnitTest
test_mainPackage = do
  res <- lift $ withSystemTempDir "hix-test" \ tmp ->
    runM root (ghciCmdlineFromOptions contextHandlers tmp mainOptions)
  cmdline <- evalEither res
  mainPackageTarget root cmdline.scriptFile === argsGhciRun cmdline

spec2 :: TargetSpec
spec2 =
  TargetForFile $ PathConcrete $ Abs (root </> [relfile|packages/core/test/Main.hs|])

spec3 :: TargetSpec
spec3 =
  TargetForComponent ComponentCoords {
    package = Just (PackageSpec "packages/core" (Just (Rel [reldir|packages/core|]))),
    component = Just (ComponentSpec "core-test" (Just (SourceDir [reldir|core-test|])))
  }

runnerFor :: EnvRunner -> TargetSpec -> UnitTest
runnerFor target spec = do
  res <- evalEither =<< liftIO (runM root (commandEnv contextHandlers commandContext opts))
  target === res.runner
  where
    opts = CommandOptions {
      root = Nothing,
      component = Just spec,
      env = Nothing
      -- (Left (EnvConfig packages defaultRunner Nothing)) Nothing (Just spec)
    }

test_componentEnv :: UnitTest
test_componentEnv = do
  runnerFor runner1 spec1
  runnerFor runner2 spec2
  runnerFor runner2 spec3

spec4 :: TargetSpec
spec4 =
  TargetForFile $ PathConcrete $ Abs (root </> [relfile|packages/core/test/Core/Test/Main.hs|])

target_moduleName :: Text
target_moduleName =
  [exon|:cd packages/core/
import Test.Tasty
:load #{m}
import #{m}|]
  where
    m = "Core.Test.Main"

test_moduleName :: UnitTest
test_moduleName = do
  conf <- evalEither =<< liftIO (runM root (assemble opts ghciContext))
  target_moduleName === conf.script
  where
    opts = options.ghci {GhciOptions.command = options.ghci.command {component = Just spec4} }

test_ghci :: TestTree
test_ghci =
  testGroup "ghci" [
    unitTest "run ghcid" test_ghcid,
    unitTest "main package" test_mainPackage,
    unitTest "component env" test_componentEnv,
    unitTest "extract module name from path" test_moduleName
  ]
