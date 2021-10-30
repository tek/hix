{
  description = "hix test project";

  inputs.hix.url = path:../../;

  outputs = { hix, ... }:
  hix.flake {
    base = ./.;
    packages = {
      root = ./.;
    };
    compat = false;
  };
}
