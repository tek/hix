{
  description = "hix test project";

  inputs.hix.url = "path:HIX";

  outputs = { hix, ... }:
  hix.lib.flake ({config, lib, ...}: let
    global = config;
  in {
    main = "root";
    managedDeps.enable = true;
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

      root = {config, ...}: {
        src = ./.;

        executables.run = {
          dependOnLibrary = false;
          main = "Run.hs";
          source-dirs = ["app"];
          dependencies = [
            global.packages.root.dep.minor
            {
              name = "sop-core";
              condition = { type = "os"; args = { os = "windows"; }; };
            }
            {
              name = "hashable";
              condition = { type = "impl"; args = { version = "9.4"; op = ">="; }; };
            }
          ];
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
            source-dirs = "src-1";
            paths = false;
          };
          lib2 = {
            public = true;
            dependencies = ["transformers" "extra" config.libraries.lib1.dep.exact];
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
