{
  description = "hix test project";

  inputs.hix.url = "path:HIX";

  outputs = { hix, ... }:
  hix.lib.flake ({config, lib, ...}: let
    global = config;
  in {
    main = "root";
    managed.enable = true;
    managed.mergeBounds = true;
    cabal = {
      dependencies = ["base >= 4 && < 5"];
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

      root = {config, ...}: {
        src = ./.;

        executables.run = {
          dependOnLibrary = false;
          main = "Run.hs";
          source-dirs = ["app"];
          dependencies = [global.packages.root.dep.minor];
        };

        library = {
          enable = true;
          source-dirs = "src";
          dependencies = [
            "aeson"
            {
              name = "array";
              version = "0.5.4.0";
              mixin = ["hiding (Data.Array)"];
            }
          ];
          paths = false;
        };

        libraries = {
          lib1 = {
            public = false;
            source-dirs = "src-1";
            paths = false;
          };
          lib2 = {
            public = true;
            dependencies = ["transformers" config.libraries.lib1.dep.exact];
          };
        };

        test = {
          enable = true;
          testSuffix = "-unit";
        };

        cabal = {
          version = "23";
          meta.executables.run.dependencies = ["root:{lib1,lib2}" "root"];
        };

      };

    };
    compat.enable = false;
  });
}
