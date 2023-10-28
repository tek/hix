{
  description = "hix test project";

  inputs.hix.url = "path:HIX";

  outputs = {hix, ...}: hix.lib.flake {
    managedDeps.enable = true;
    packages = {
      root = {
        src = ./.;
        library = {
          enable = true;
          dependencies = [
            "aeson"
            "extra"
            "th-abstraction ^>= 0.5"
          ];
        };
        test.enable = true;
      };
    };
    overrides = {hackage, ...}: {
      th-abstraction = hackage "0.5.0.0" "0dkilfrvk8zdn3gvyfv5zgjbwqhdf1yg90fk4byka0ib43kgkyvf";
    };
    envs.latest.overrides = {hackage, ...}: {
      aeson = hackage "2.0.3.0" "1yqw1glxv3lh5759f7vrn2bq8ih9k5m9j56b34a61dxx07b1x1jq";
    };
    # TODO remove
    internal.hixCli.dev = true;
  };
}
