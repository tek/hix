{
  description = "hix test project";

  inputs.hix.url = "HIX";

  outputs = {hix, ...}: hix.lib._hix_test {

    hackage = {
      askVersion = false;
      confirm = false;
      check = false;

      repos = {

        "hackage.haskell.org" = {
          publish = false;
          indexState = "2024-11-01T00:00:00Z";
        };

        local = {
          publish = true;
          user = "test";
          password = "test";
        };

      };
    };

    managed = {
      enable = true;
      sets = {
        main = ["local1"];
        other = ["local2"];
      };
      latest.compiler = "ghc98";
      forceBounds.base.upper = "5";
    };

    ui.experimental.managed-maint = true;

    compat.enable = false;

    overrides = {hackage, ...}: {
      extra = hackage "1.7.16" "0y27q0zas66qbgvjga0w8dmfjcs0kcn9nwps17iyd60ij3yqivhj";
      semigroups = hackage "0.19.2" "1q8ssd9c3rr1d0vjc6vdnl27r9n0p3xxacag6s0ahsa2njz225ra";
      path = hackage "0.8.0" "0isldidz2gypw2pz399g6rn77x9mppd1mvj5h6ify4pj4mpla0pb";
    };

    packages = {
      local1 = {
        src = ./packages/local1;
        versionFile = "ops/version-local1.nix";
        library = {
          enable = true;
          dependencies = ["extra"];
        };
      };
      local2 = {
        src = ./packages/local2;
        versionFile = "ops/version-local2.nix";
        library = {
          enable = true;
          dependencies = ["semigroups"];
        };
      };
    };

    # TODO remove
    internal.hixCli.dev = false;

  };
}
