{
  description = "hix test dep 1";

  inputs.hix.url = "path:HIX";

  outputs = { hix, ... }:
  hix.lib._hix_test {
    packages.dep1 = {
      src = ./.;
      library.enable = true;
    };
    compat.enable = false;
  };
}
