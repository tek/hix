{
  description = "hix test project";

  inputs.hix.url = "path:HIX";

  outputs = { hix, ... }:
  hix.lib.flake ({ config, ... }: {
    packages.root = {
      src = ./.;
      library.enable = true;
      executable.enable = true;
    };
    compiler = "ghc94";
    envs.dev.ghc.crossPkgs = config.envs.dev.ghc.pkgs.pkgsCross.musl64;
    overrides = { hsLib, self, ... }: {
      static = hsLib.justStaticExecutables self.root;
    };
    output.extraPackages = ["static"];
  });
}
