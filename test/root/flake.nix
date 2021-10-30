{
  description = "hix test project";

  inputs.hix.url = path:../../;

  outputs = { hix, ... }:
  hix.flake {
    base = ./.;
    main = "root";
    packages = {
      root = ./.;
      sub = ./sub;
    };
    compat = false;
  };
}
