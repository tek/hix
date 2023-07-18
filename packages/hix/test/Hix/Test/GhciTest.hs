module Hix.Test.GhciTest where

import Control.Monad.Trans.Class (lift)
import Exon (exon)
import Hedgehog (TestT, evalEither, (===))
import Path (Abs, Dir, File, Path, Rel, SomeBase (Rel), absdir, absfile, reldir, relfile, (</>))
import Path.IO (withSystemTempDir)

import Hix.Data.ComponentConfig (
  ComponentConfig (..),
  ComponentName,
  EnvRunner (EnvRunner),
  PackageConfig (..),
  PackagesConfig,
  SourceDir (SourceDir),
  SourceDirs (SourceDirs),
  )
import Hix.Data.Error (pathText)
import Hix.Data.GhciConfig (ChangeDir (ChangeDir), EnvConfig (EnvConfig), GhciConfig (..))
import qualified Hix.Data.GhciTest as GhciTest
import Hix.Env (envRunner)
import Hix.Ghci (assemble, ghcidCmdlineFromOptions)
import Hix.Monad (runM)
import qualified Hix.Options as Options
import Hix.Options (
  ComponentCoords (ComponentCoords),
  ComponentSpec (ComponentSpec),
  EnvRunnerOptions (EnvRunnerOptions),
  GhciOptions (GhciOptions, component),
  GhcidOptions (GhcidOptions),
  PackageSpec (PackageSpec),
  TargetSpec (TargetForComponent, TargetForFile),
  TestOptions (TestOptions),
  )

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

component :: ComponentName -> Path Rel Dir -> EnvRunner -> ComponentConfig
component name dir runner =
  ComponentConfig {
    name,
    sourceDirs = SourceDirs [SourceDir dir],
    runner = Just runner,
    extensions = [],
    language = "GHC2021",
    ghcOptions = [],
    prelude = Nothing
  }

packages :: PackagesConfig
packages =
  [
    ("api", PackageConfig {
      name = "api",
      src = [reldir|packages/api|],
      components = [
        ("library", component "api" [reldir|lib|] runner1),
        ("server", component "server" [reldir|app|] runner1),
        ("api-test", component "api-test" [reldir|test|] runner1)
      ]
    }),
    ("core", PackageConfig {
      name = "core",
      src = [reldir|packages/core|],
      components = [
        ("library", component "core" [reldir|lib|] runner1),
        ("core-test", component "core-test" [reldir|test|] runner2)
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
      setup = [("generic", "import Test.Tasty")],
      run = [("generic", ("check . property . test"))],
      args = ["-Werror"]
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

ghcidTarget ::
  Path Abs Dir ->
  Path Abs File ->
  Text
ghcidTarget cwd scriptFile =
  [exon|ghcid --command="ghci -Werror -i#{path} -ghci-script=#{pathText scriptFile}" --test='#{test}'|]
  where
    test = "(check . property . test) test_server"
    path = [exon|#{dir}packages/api/test/:#{dir}packages/api/lib/:#{dir}packages/core/lib/|]
    dir = pathText cwd

test_ghcid :: TestT IO ()
test_ghcid = do
  res <- lift $ withSystemTempDir "hix-test" \ tmp ->
    runM root (ghcidCmdlineFromOptions tmp options)
  cmdline <- evalEither res
  ghcidTarget root cmdline.ghci.scriptFile === cmdline.cmdline

spec2 :: TargetSpec
spec2 =
  TargetForFile (root </> [relfile|packages/core/test/Main.hs|])

spec3 :: TargetSpec
spec3 =
  TargetForComponent ComponentCoords {
    package = Just (PackageSpec "packages/core" (Just (Rel [reldir|packages/core|]))),
    component = Just (ComponentSpec "core-test" (Just (SourceDir [reldir|core-test|])))
  }

runnerFor :: EnvRunner -> TargetSpec -> TestT IO ()
runnerFor target spec = do
  res <- evalEither =<< liftIO (runM root (envRunner conf))
  target === res
  where
    conf = EnvRunnerOptions (Left (EnvConfig packages defaultRunner)) Nothing (Just spec)

test_componentEnv :: TestT IO ()
test_componentEnv = do
  runnerFor runner1 spec1
  runnerFor runner2 spec2
  runnerFor runner2 spec3

spec4 :: TargetSpec
spec4 =
  TargetForFile (root </> [relfile|packages/core/test/Core/Test/Main.hs|])

target_moduleName :: Text
target_moduleName =
  [exon|:cd packages/core/
import Test.Tasty
:load #{m}
import #{m}|]
  where
    m = "Core.Test.Main"

test_moduleName :: TestT IO ()
test_moduleName = do
  conf <- evalEither =<< liftIO (runM root (assemble options.ghci { component = spec4 }))
  target_moduleName === conf.script
