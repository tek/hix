{
  description = "hix test project";

  inputs.hix.url = "HIX";

  outputs = {hix, ...}: hix.lib._hix_test {

    managed = {
      enable = true;
      verbose = false;
      debug = false;
      quiet = true;
      sets = "each";
      lower = {
        enable = true;
        compiler = "ghc90";
      };
    };

    ghcVersions = [];
    compat.enable = false;

    packages = {
      local1 = {
        src = ./packages/local1;
        library = {
          enable = true;
          dependencies = [
            "containers <0.6"
          ];
        };
      };
      local2 = {
        src = ./packages/local2;
        library = {
          enable = true;
          dependencies = [
            "local1"
            "semigroups"
          ];
        };
      };
    };

    envs.lower-local1.localPackage = api: api.minimal;
    envs.lower-local2.localPackage = api: api.minimal;

    internal.hixCli.dev = false;

  };
}
