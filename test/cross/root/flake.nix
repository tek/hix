{
  description = "hix test project";

  inputs.hix.url = path:HIX;

  outputs = { hix, ... }:
  hix.lib.flake ({ config, ... }: {
    packages.root = ./.;
    devGhc.compiler = "ghc902";
    devGhc.crossPkgs = config.devGhc.pkgs.pkgsCross.musl64;
    overrides = { hsLib, self, ... }: {
      static = hsLib.justStaticExecutables self.root;
    };
    output.extraPackages = ["static"];
  });
}
