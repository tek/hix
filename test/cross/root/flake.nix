{
  description = "hix test project";

  inputs.hix.url = "path:HIX";

  outputs = {hix, ...}:
  hix.lib._hix_test ({config, ...}: {
    packages.root = {
      src = ./.;
      cabal.dependencies = ["text"];
      library.enable = true;
      executable = {
        enable = true;
        name = "exe";
        source-dirs = "app";
      };
      executables.alias = {
        enable = true;
        source-dirs = "app";
      };
    };
    envs.cross = {
      expose = true;
      ghc.crossPkgs = config.envs.dev.ghc.pkgs.pkgsCross.musl64;
    };
    overrides = { hsLib, self, ... }: {
      static-override = hsLib.justStaticExecutables self.root;
    };
    output.extraPackages = ["static-override"];
  });
}
