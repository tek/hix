{type, util, default ? true}: let

  inherit (util.lib) mkOption types;

in
{...}: {

  options = {

    packages = mkOption {
      description = "Whether to expose this ${type} in [](#opt-general-outputs.packages).";
      type = types.bool;
      inherit default;
    };

    apps = mkOption {
      description = "Whether to expose this ${type} in [](#opt-general-outputs.apps) if it has executables.";
      type = types.bool;
      inherit default;
    };

    checks = mkOption {
      description = "Whether to expose this ${type} in [](#opt-general-outputs.checks).";
      type = types.bool;
      inherit default;
    };

    scoped = mkOption {
      description = ''
      Whether to expose this ${type} in [](#opt-general-outputs.legacyPackages) in env-keyed subsets like
      `ghc98.<pkgname>`.
      These are not evaluated by `nix flake check` and only buildable when referring to the whole path, like
      `nix build .#ghc98.<pkgname>`, so they can include packages you don't want evaluated or built frequently
      (because they pull in expensive dependencies etc).
      '';
      type = types.bool;
      inherit default;
    };

  };

}
