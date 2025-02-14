{
  description = "hix test project";

  inputs.hix.url = "path:HIX";

  outputs = {self, hix, ...}: hix.lib._hix_test ({config, lib, ...}: {
    managed = {
      enable = true;
      verbose = false;
      debug = false;
      quiet = true;
      sets = {
        main = ["local1"];
        other = ["local2" "local3"];
      };
      latest.compiler = "ghc98";
      forceBounds.base.upper = "5";
    };
    compat.enable = false;
    packages = {
      local1 = {
        src = ./packages/local1;
        library = {
          enable = true;
          dependencies = ["aeson"];
        };
        test = {
          enable = true;
          dependencies = ["extra"];
        };
      };
      local2 = {
        src = ./packages/local2;
        library = {
          enable = true;
          dependencies = ["path"];
        };
      };
      local3 = {
        src = ./packages/local3;
        library = {
          enable = true;
          dependencies = ["path" config.packages.local2.dep.exact];
        };
      };
    };

    # This should not be evaluated, since only env-local overrides are used for managed envs
    overrides = {...}: {
      extra = throw "global overrides";
    };
  });
}
