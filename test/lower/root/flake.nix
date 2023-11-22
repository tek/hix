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
        test.enable = true;
      };
    };

    envs.lower-main.localPackage = api: api.minimal;

    # TODO remove
    internal.hixCli.dev = true;
  });
}
