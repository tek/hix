{
  description = "hix test project for release with managed.enable = false";

  inputs.hix.url = "HIX";

  outputs = {hix, ...}: hix.lib._hix_test {

    release.versionFile = "version.nix";

    managed = {
      enable = false;
    };

    internal.hixCli.dev = true;

    compat.enable = false;

    packages = {
      local1 = {
        src = ./packages/local1;
        library = {
          enable = true;
        };
      };
      local2 = {
        src = ./packages/local2;
        library = {
          enable = true;
        };
      };
    };

  };
}
