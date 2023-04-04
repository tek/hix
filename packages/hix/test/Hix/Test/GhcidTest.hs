module Hix.Test.GhcidTest where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (runReaderT)
import Exon (exon)
import Hedgehog (TestT, evalEither, (===))
import Path (Abs, Dir, File, Path, Rel, SomeBase (Rel), absdir, absfile, reldir, relfile, (</>))
import Path.IO (getCurrentDir, withSystemTempDir)

import Hix.Data.Error (pathText)
import Hix.Data.GhciConfig (
  ComponentConfig (..),
  ComponentName,
  EnvConfig (EnvConfig),
  EnvRunner (EnvRunner),
  GhciConfig (..),
  PackageConfig (..),
  PackagesConfig,
  SourceDir (SourceDir),
  SourceDirs (SourceDirs),
  )
import qualified Hix.Data.GhcidTest as GhcidTest
import Hix.Env (envRunner)
import Hix.Ghci (ghcidCmdlineFromOptions)
import Hix.Monad (Env (Env), runM)
import qualified Hix.Options as Options
import Hix.Options (
  ComponentCoords (ComponentCoords),
  ComponentSpec (ComponentSpec),
  EnvRunnerOptions (EnvRunnerOptions),
  GhciOptions (GhciOptions),
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
    runner = Just runner
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
    package = PackageSpec "api" (Just (Rel [reldir|api|])),
    component = ComponentSpec "test" (Just (SourceDir [reldir|test|]))
  }

options :: GhciOptions
options =
  GhciOptions {
    config = GhciConfig {
      packages,
      setup = [("generic", "import Test.Tasty")],
      run = [("generic", ("check . property . test"))],
      args = ["-Werror"]
    },
    component = spec1,
    test = TestOptions {
      mod = "Api.ServerTest",
      test = Just "test_server",
      runner = Just "generic"
    }
  }

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
  cwd <- getCurrentDir
  res <- lift $ withSystemTempDir "hix-test" \ tmp ->
    runM root (ghcidCmdlineFromOptions tmp options)
  cmdline <- evalEither res
  ghcidTarget cwd cmdline.ghci.scriptFile === cmdline.cmdline

spec2 :: TargetSpec
spec2 =
  TargetForFile (root </> [relfile|packages/core/test/Main.hs|])

spec3 :: TargetSpec
spec3 =
  TargetForComponent ComponentCoords {
    package = PackageSpec "packages/core" (Just (Rel [reldir|packages/core|])),
    component = ComponentSpec "core-test" (Just (SourceDir [reldir|core-test|]))
  }

runnerFor :: EnvRunner -> TargetSpec -> TestT IO ()
runnerFor target spec = do
  res <- evalEither =<< liftIO (runExceptT (runReaderT (envRunner conf) (Env root)))
  target === res
  where
    conf = EnvRunnerOptions (EnvConfig packages defaultRunner) (Just spec)

test_componentEnv :: TestT IO ()
test_componentEnv = do
  runnerFor runner1 spec1
  runnerFor runner2 spec2
  runnerFor runner2 spec3
