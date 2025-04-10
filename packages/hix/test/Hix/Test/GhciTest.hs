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
import Hix.Data.GhciConfig (ChangeDir (ChangeDir), EnvConfig (EnvConfig), GhciConfig (..))
import qualified Hix.Data.GhciTest as GhciTest
import qualified Hix.Data.Options as Options
import Hix.Data.Options (
  ComponentCoords (ComponentCoords),
  ComponentSpec (ComponentSpec),
  EnvRunnerOptions (EnvRunnerOptions),
  GhciOptions (GhciOptions, component),
  GhcidOptions (GhcidOptions),
  PackageSpec (PackageSpec),
  TargetSpec (TargetForComponent, TargetForFile),
  TestOptions (TestOptions),
  )
import Hix.Data.PathSpec (PathSpec (PathConcrete))
import Hix.Env (envRunner)
import Hix.Error (pathText)
import Hix.Ghci (assemble, ghciCmdlineFromOptions, ghcidCmdlineFromOptions)
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

component :: ComponentName -> Path Rel Dir -> Set Dependency -> EnvRunner -> ComponentConfig
component name dir deps runner =
  ComponentConfig {
    name,
    sourceDirs = SourceDirs [SourceDir dir],
    runner = Just runner,
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
        ("library", component "api" [reldir|lib|] ["core:tools"] runner1),
        ("testing", component "testing" [reldir|testing|] ["api:testing"] runner1),
        ("server", component "server" [reldir|app|] ["api"] runner1),
        ("api-test", component "api-test" [reldir|test|] ["api:testing", "api"] runner1)
      ]
    }),
    ("core", PackageConfig {
      name = "core",
      src = [reldir|packages/core|],
      components = [
        ("library", component "core" [reldir|lib|] ["core"] runner1),
        ("tools", component "tools" [reldir|tools|] ["core"] runner2),
        ("core-test", component "core-test" [reldir|test|] ["core"] runner2)
      ]
    })
  ]

spec1 :: TargetSpec
spec1 =
  TargetForComponent ComponentCoords {
    package = Just (PackageSpec "api" (Just (Rel [reldir|api|]))),
    component = Just (ComponentSpec "test" (Just (SourceDir [reldir|test|])))
  }

ghciOptions :: GhciOptions
ghciOptions =
  GhciOptions {
    config = Left GhciConfig {
      packages,
      mainPackage = Nothing,
      setup = [("generic", "import Test.Tasty")],
      run = [("generic", "check . property . test")],
      args = ["-Werror"],
      manualCabal = False
    },
    root = Nothing,
    component = spec1,
    test = TestOptions {
      mod = "Api.ServerTest",
      test = Just "test_server",
      runner = Just "generic",
      cd = ChangeDir True
    },
    extra = Nothing
  }

options :: GhcidOptions
options =
  GhcidOptions {ghci = ghciOptions, extra = Nothing}

searchPath :: Text -> [Text] -> Text
searchPath dir subs =
  Text.intercalate ":" [[exon|#{dir}packages/#{sub}/|] | sub <- subs]

ghcidTarget ::
  Path Abs Dir ->
  Path Abs File ->
  Text
ghcidTarget cwd scriptFile =
  [exon|ghcid --command="ghci -Werror -i#{path} -ghci-script=#{pathText scriptFile}" --test='#{test}'|]
  where
    test = "(check . property . test) test_server"
    path = searchPath dir ["api/test", "api/lib", "api/testing", "core/lib", "core/tools"]
    dir = pathText cwd

test_ghcid :: UnitTest
test_ghcid = do
  res <- lift $ withSystemTempDir "hix-test" \ tmp ->
    runM root (ghcidCmdlineFromOptions tmp options)
  cmdline <- evalEither res
  ghcidTarget root cmdline.ghci.scriptFile === cmdline.cmdline

mainOptions :: GhciOptions
mainOptions =
  GhciOptions {
    config = Left GhciConfig {
      packages,
      mainPackage = Just "core",
      setup = [("generic", "import Test.Tasty")],
      run = [("generic", "")],
      args = [],
      manualCabal = False
    },
    root = Nothing,
    component = TargetForComponent (ComponentCoords Nothing Nothing),
    test = TestOptions {
      mod = "Main",
      test = Nothing,
      runner = Nothing,
      cd = ChangeDir True
    },
    extra = Nothing
  }

mainPackageTarget ::
  Path Abs Dir ->
  Path Abs File ->
  Text
mainPackageTarget cwd scriptFile =
  [exon|ghci -i#{path} -ghci-script=#{pathText scriptFile}|]
  where
    path = searchPath dir ["core/test", "core/lib"]
    dir = pathText cwd

test_mainPackage :: UnitTest
test_mainPackage = do
  res <- lift $ withSystemTempDir "hix-test" \ tmp ->
    runM root (ghciCmdlineFromOptions tmp mainOptions)
  cmdline <- evalEither res
  mainPackageTarget root cmdline.scriptFile === cmdline.cmdline

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
  res <- evalEither =<< liftIO (runM root (envRunner conf))
  target === res
  where
    conf = EnvRunnerOptions (Left (EnvConfig packages defaultRunner Nothing)) Nothing (Just spec)

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
  conf <- evalEither =<< liftIO (runM root (assemble options.ghci { component = spec4 }))
  target_moduleName === conf.script

test_ghci :: TestTree
test_ghci =
  testGroup "ghci" [
    unitTest "run ghcid" test_ghcid,
    unitTest "main package" test_mainPackage,
    unitTest "component env" test_componentEnv,
    unitTest "extract module name from path" test_moduleName
  ]
