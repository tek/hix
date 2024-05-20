{
  description = "hix test project";

  inputs.hix.url = "path:HIX";

  outputs = {self, hix, ...}: hix.lib.flake ({config, lib, ...}: {
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

    # TODO remove
    internal.hixCli.dev = false;
  });
}
