{
  description = "hix test project";

  inputs.hix.url = path:HIX;

  outputs = { hix, ... }:
  hix.lib.flake {
    packages.root = {
      src = ./.;
      cabal.dependencies = ["aeson"];
      library.enable = true;
      library.dependencies = ["messagepack"];
      executables.root = { enable = true; source-dirs = "."; };
      test.enable = true;
    };
    cabal.dependencies = ["base" "some"];
  };
}
