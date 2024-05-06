{
  description = "hix test project";

  inputs.hix.url = "path:HIX";

  outputs = {hix, ...}:
  hix.lib.flake ({config, ...}: {
    packages.root = {
      src = ./.;
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
    envs.cross.ghc.crossPkgs = config.envs.dev.ghc.pkgs.pkgsCross.musl64;
    overrides = { hsLib, self, ... }: {
      static-override = hsLib.justStaticExecutables self.root;
    };
    output.extraPackages = ["static-override"];
  });
}
