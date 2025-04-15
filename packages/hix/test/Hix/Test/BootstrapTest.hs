module Hix.Test.BootstrapTest where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Exon (exon)
import Hedgehog (evalEither, (===))
import Path (parent, reldir, relfile, toFilePath, (</>))
import Path.IO (createDirIfMissing, withSystemTempDir)

import Hix.Bootstrap (bootstrapFiles)
import qualified Hix.Data.BootstrapProjectConfig
import Hix.Data.BootstrapProjectConfig (BootstrapProjectConfig (BootstrapProjectConfig))
import qualified Hix.Data.ProjectFile as ProjectFile
import Hix.Monad (runM)
import Hix.Test.Utils (UnitTest)

cabal :: Text
cabal =
  [exon|cabal-version: 2.2

name:           red-panda
version:        0.1.0.0
synopsis:       A Haskell "project"
description:    See https://hackage.haskell.org/package/red-panda/docs/RedPanda.html
author:         Panda
maintainer:     Panda
license:        BSD-2-Clause-Patent
license-file:   LICENSE
build-type:     Simple

library
  exposed-modules:
      RedPanda
  other-modules:
      NormalPanda
      Paths_red_panda
  autogen-modules:
      Paths_red_panda
  hs-source-dirs:
      lib
  ghc-options: -Wall
  build-depends:
      base ==4.*
    , incipit-base >= 0.4 && < 0.6
    , containers
  default-language: Haskell2010
  default-extensions:
      OverloadedStrings
    , OverloadedRecordDot
  mixins:
      base hiding (Prelude)
    , incipit-base (IncipitBase as Prelude)
    , incipit-base hiding (IncipitBase)
  reexported-modules:
      Control.Monad
    , Data.Maybe

executable red-panda
  main-is: Main.hs
  hs-source-dirs:
      app
  ghc-options: -threaded -rtsopts -with-rtsopts=-N -Wall
  build-depends:
      base ==4.*
    , red-panda
  default-language: Haskell2010

test-suite red-panda-test
  type: exitcode-stdio-1.0
  main-is: Main.hs
  other-modules:
      RedPanda.Test.NameTest
  hs-source-dirs:
      test
  ghc-options: -threaded -rtsopts -with-rtsopts=-N -Wall
  build-depends:
      base ==4.*
    , hedgehog >=1.1 && <1.5
    , red-panda
    , tasty ==1.4.*
    , tasty-hedgehog >=1.3 && <1.5
  default-language: Haskell2010
|]

conf :: BootstrapProjectConfig
conf =
  BootstrapProjectConfig {hixUrl = def, noInitGitAndFlake = True, devCli = False}

flakeTarget :: Text
flakeTarget =
  [exon|{
  description = "A Haskell project";
  inputs.hix.url = "github:tek/hix";
  outputs = {hix, ...}: hix.lib.flake {
    packages = {
      red-panda = {
        src = ./packages/red-panda;
        description = "See https://hackage.haskell.org/package/red-panda/docs/RedPanda.html";
        cabal = {
          author = "Panda";
          build-type = "Simple";
          license = "BSD-2-Clause-Patent";
          license-file = "LICENSE";
          version = "0.1.0.0";
          meta = {
            maintainer = "Panda";
            synopsis = "A Haskell \"project\"";
          };
        };
        library = {
          enable = true;
          prelude = {
            package = {
              name = "incipit-base";
              version = ">=0.4 && <0.6";
            };
            module = "IncipitBase";
          };
          dependencies = [
            "containers"
            "base >=4 && <5"
          ];
          default-extensions = [
            "OverloadedStrings"
            "OverloadedRecordDot"
          ];
          source-dirs = "lib";
          language = "Haskell2010";
          ghc-options = [
            "-Wall"
          ];
          component = {
            other-modules = [
              "NormalPanda"
              "Paths_red_panda"
            ];
          };
          reexported-modules = [
            "Control.Monad"
            "Data.Maybe"
          ];
        };
        executables.red-panda = {
          dependencies = [
            "red-panda"
          ];
          source-dirs = "app";
          language = "Haskell2010";
          ghc-options = [
            "-Wall"
          ];
        };
        tests.red-panda-test = {
          dependencies = [
            "hedgehog >=1.1 && <1.5"
            "red-panda"
            "tasty >=1.4 && <1.5"
            "tasty-hedgehog >=1.3 && <1.5"
          ];
          source-dirs = "test";
          language = "Haskell2010";
          ghc-options = [
            "-Wall"
          ];
          component = {
            other-modules = [
              "RedPanda.Test.NameTest"
            ];
          };
        };
      };
    };
  };
}
|]

test_bootstrap :: UnitTest
test_bootstrap = do
  result <- liftIO $ withSystemTempDir "hix-unit" \ tmp -> do
    let
      root = tmp </> [reldir|red-panda|]
      cabalFile = root </> [relfile|packages/red-panda/red-panda.cabal|]
    createDirIfMissing True (parent cabalFile)
    Text.writeFile (toFilePath cabalFile) cabal
    runM root (bootstrapFiles conf)
  [flake] <- evalEither result
  Text.lines flakeTarget === Text.lines flake.content
