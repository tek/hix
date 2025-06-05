{
  description = "hix test project";

  inputs.hix.url = "HIX";

  outputs = {hix, ...}: hix.lib._hix_test {

    hackage = {
      repos = {

        "hackage.haskell.org" = {
          publish = false;
          indexState = "2024-11-01T00:00:00Z";
        };

        local = {
          publish = true;
          user = "test";
          password = "test";
          location = "http://localhost:HACKAGE_PORT";
        };

      };
    };

    managed = {
      enable = true;
      sets = "each";
      latest.compiler = "ghc910";
    };

    release = let

        hook = ''
        if [[ $phase == 'post-upload' ]]
        then
          echo $version > new-version
        fi
        '';

    in {

      setChangelogVersion = true;
      hooks = [hook];

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
