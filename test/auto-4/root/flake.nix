{
  description = "hix test project";

  inputs.hix.url = path:HIX;

  outputs = { hix, ... }:
  hix.lib.auto {
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
    devGhc.compiler = "ghc902";
  };
}
