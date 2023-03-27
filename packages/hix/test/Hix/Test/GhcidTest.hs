module Hix.Test.GhcidTest where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT)
import Exon (exon)
import Hedgehog (TestT, evalEither, (===))
import Hix.Data.Error (pathText)
import Hix.Data.GhciConfig (
  ComponentConfig (..),
  ComponentName,
  GhciConfig (..),
  PackageConfig (..),
  SourceDir (SourceDir),
  SourceDirs (SourceDirs),
  )
import qualified Hix.Data.GhcidTest as GhcidTest
import Hix.Ghci (ghcidCmdlineFromOptions, ghcidEnv)
import qualified Hix.Options as Options
import Hix.Options (GhciOptions (GhciOptions), GhcidModuleSpec (GhcidModuleSpec))
import Path (Abs, Dir, File, Path, Rel, absfile, reldir)
import Path.IO (getCurrentDir, withSystemTempDir)

component :: ComponentName -> Path Rel Dir -> ComponentConfig
component name dir =
  ComponentConfig {name, sourceDirs = SourceDirs [SourceDir dir], runner = [absfile|/tmp/hix/test/env.bash|]}

options :: GhciOptions
options =
  GhciOptions {
    config = GhciConfig {
      packages = [
        ("api", PackageConfig {
          name = "api",
          src = [reldir|packages/api|],
          components = [
            ("library", component "api" [reldir|lib|]),
            ("server", component "server" [reldir|app|]),
            ("api-test", component "api-test" [reldir|test|])
          ]
        }),
        ("core", PackageConfig {
          name = "core",
          src = [reldir|packages/core|],
          components = [
            ("library", component "core" [reldir|lib|]),
            ("core-test", component "core-test" [reldir|test|])
          ]
        })
      ],
      setup = [("generic", "import Test.Tasty")],
      run = [("generic", ("check . property . test"))],
      args = ["-Werror"]
    },
    spec = Right GhcidModuleSpec {
      package = "api",
      module_ = "Api.ServerTest",
      sourceDir = SourceDir [reldir|test|]
    },
    test = Just "test_server",
    runner = "generic"
  }

target ::
  Path Abs Dir ->
  Path Abs File ->
  Text
target cwd scriptFile =
  [exon|ghcid --command="ghci -Werror -i#{dir}packages/api/test/:#{dir}packages/api/lib/:#{dir}packages/core/lib/ -ghci-script=#{pathText scriptFile}" --test='(check . property . test) test_server'|]
  where
    dir = pathText cwd

test_ghcid :: TestT IO ()
test_ghcid = do
  cwd <- getCurrentDir
  res <- lift $ withSystemTempDir "hix-test" \ tmp ->
    runExceptT (ghcidCmdlineFromOptions tmp options)
  cmdline <- evalEither res
  target cwd cmdline.ghci.scriptFile === cmdline.cmdline

test_componentEnv :: TestT IO ()
test_componentEnv =
  dbgs =<< liftIO (runExceptT (ghcidEnv options))
