{
  description = "hix test project";

  inputs.hix.url = path:HIX;

  outputs = { hix, ... }:
  hix.lib.flake {
    packages.root = {
      src = ./.;
      cabal.dependencies = ["aeson"];
      executable = {
        enable = true;
        source-dirs = ".";
      };
    };
    cabal.dependencies = ["base" "polysemy"];
    compat.enable = false;
    envs.dev.ghc.compiler = "ghc90";
  };
}
