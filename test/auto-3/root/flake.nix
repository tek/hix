{
  description = "hix test project";

  inputs.hix.url = path:HIX;

  outputs = { hix, ... }:
  hix.lib.auto {
    packages.root = ./.;
    compat.enable = false;
    devGhc.compiler = "ghc902";
  };
}
