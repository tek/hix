{
  description = "hix test project";

  inputs.hix.url = "HIX";

  outputs = {self, hix, ...}: hix.lib._hix_test ({config, lib, ...}: {

    managed = {
      enable = true;
      lower = {
        enable = true;
        compiler = "ghc90";
      };
      verbose = false;
      debug = false;
      quiet = true;
      sets = { main = ["root"]; };
    };

    ghcVersions = [];
    compat.enable = false;
    gen-overrides.enable = true;

    packages = {
      root = {
        src = ./.;
        library = {
          enable = true;
          dependencies = [
            "aeson >=2.0 && <2.3"
            "extra >=1.7 && <1.8"
          ];
        };
      };
    };

    envs.lower-main.localPackage = api: api.minimal;

  });
}
