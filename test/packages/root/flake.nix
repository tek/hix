{
  description = "hix test project";

  inputs.hix.url = path:HIX;

  outputs = { hix, ... }:
  hix.lib.flake ({lib, ...}: {
    main = "root";
    ghcVersions = ["ghc925"];
    cabal = {
      dependencies = ["base >= 4 && < 6"];
      ghc-options = ["-Wunused-imports"];
      default-extensions = ["DataKinds"];
      version = lib.mkDefault "13";
      meta.author = "Author McCodeface";
      component.language = "GHC2021";
    };
    packages = {

      dep-lib = {
        src = ./dep;
        cabal = {
          ghc-options = ["-Wall" "-Werror"];
          default-extensions = ["OverloadedRecordDot"];
          dependencies = ["containers"];
        };
        library = {
          enable = true;
          source-dirs = "lib";
          reexported-modules = ["Data.Set"];
        };
      };

      root = {
        src = ./.;

        executables.run = {
          main = "Run.hs";
          source-dirs = ["app"];
        };

        library = {
          enable = true;
          source-dirs = "src";
          dependencies = ["aeson" { name = "incipit"; version = 5; mixin = "hiding (Prelude)"; }];
          paths = false;
        };

        test = {
          enable = true;
          testSuffix = "-unit";
        };

        cabal = {
          version = "23";
          meta.executables.run.dependencies = ["dep" "root"];
        };

      };

    };
    compat.enable = false;
  });
}
