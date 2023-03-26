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
import Hix.Ghci (withGhcidCmdline, ghcidEnv)
import qualified Hix.Options as Options
import Hix.Options (GhcidModuleSpec (GhcidModuleSpec), GhciOptions (GhciOptions))
import Path (Abs, Dir, File, Path, Rel, absdir, absfile, reldir)
import Path.IO (withSystemTempDir)

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
          src = [absdir|/tmp/hix-test/packages/api|],
          components = [
            ("library", component "api" [reldir|lib|]),
            ("server", component "server" [reldir|app|]),
            ("api-test", component "api-test" [reldir|test|])
          ]
        }),
        ("core", PackageConfig {
          name = "core",
          src = [absdir|/tmp/hix-test/packages/core|],
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
    test = "test_server",
    runner = "generic"
  }

target :: Path Abs File -> Text
target scriptFile =
  [exon|ghcid --command="ghci -Werror -i/tmp/hix-test/packages/api/test/:/tmp/hix-test/packages/api/lib/:/tmp/hix-test/packages/core/lib/ -ghci-script=#{pathText scriptFile}" --test='(check . property . test) test_server'|]

test_ghcid :: TestT IO ()
test_ghcid = do
  res <- lift $ withSystemTempDir "hix-test" \ tmp ->
    runExceptT (withGhcidCmdline tmp options \ cmdline -> pure cmdline)
  cmdline <- evalEither res
  target cmdline.scriptFile === cmdline.cmdline

test_componentEnv :: TestT IO ()
test_componentEnv =
  dbgs =<< liftIO (runExceptT (ghcidEnv options))
