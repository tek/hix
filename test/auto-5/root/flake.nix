{
  description = "hix test project";

  inputs.hix.url = path:HIX;

  outputs = { hix, ... }:
  hix.lib.auto {
    packages.root = {
      src = ./.;
      dependencies = ["aeson"];
      library.dependencies = ["messagepack"];
      executables.root.main = "Main.hs";
      tests.root-test.main = "Main.hs";
    };
    dependencies = ["base" "polysemy"];
    compat.enable = false;
    devGhc.compiler = "ghc902";
  };
}
