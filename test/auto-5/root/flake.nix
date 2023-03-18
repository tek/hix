{
  description = "hix test project";

  inputs.hix.url = path:HIX;

  outputs = { hix, ... }:
  hix.lib.auto {
    packages.root = {
      src = ./.;
      cabal.dependencies = ["aeson"];
      library.enable = true;
      library.dependencies = ["messagepack"];
      executables.root.enable = true;
      test.enable = true;
    };
    cabal.dependencies = ["base" "polysemy"];
    compat.enable = false;
    devGhc.compiler = "ghc902";
  };
}
