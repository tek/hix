{
  description = "hix test dep 2";

  inputs.hix.url = path:HIX;

  outputs = { hix, ... }:
  hix.lib.flake {
    packages.dep2 = {
      src = ./.;
      library.enable = true;
    };
    compat.enable = false;
    overrides = {
      dev = { hackage, ... }: {
        stm-chans = hackage "1.0.0" "13ml9byhsid341z9y52dav7nxv479n0j0w2qmsbj0rglz59aicgp";
      };
    };
  };
}
