{
  description = "hix test dep 1";

  inputs.hix.url = path:HIX;

  outputs = { hix, ... }:
  hix.lib.flake {
    base = ./.;
    packages.dep1 = ./.;
    compat.enable = false;
  };
}
