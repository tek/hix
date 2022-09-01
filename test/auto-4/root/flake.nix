{
  description = "hix test project";

  inputs.hix.url = path:HIX;

  outputs = { hix, ... }:
  hix.lib.auto {
    packages.root = { src = ./.; dependencies = ["aeson"]; };
    dependencies = ["base" "polysemy"];
    compat.enable = false;
    devGhc.compiler = "ghc902";
  };
}
