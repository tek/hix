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
      sets = "all";
    };

    ghcVersions = [];
    compat.enable = false;

    packages = {
      root = {
        src = ./.;
        library = {
          enable = true;
          dependencies = [
            "microlens"
            "semigroups"
          ];
        };
      };
    };

    envs.lower.localPackage = api: api.minimal;

  });
}
