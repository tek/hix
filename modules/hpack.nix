{ lib, config, util, ... }:
let
  inherit (lib) types mkOption;

in {
  options = {

    defaultApp = mkOption {
      type = types.str;
      description = ''
      The name of an executable in [](#opt-general-packages) that should be assigned to `packages.default`.
      '';
    };

    hpack = {

      script = mkOption {
        type = types.path;
        description = ''
          The script that generates a Cabal file in each of the directories configured in [](#opt-general-packages) by
          executing `hpack`.
          It is intended to be run manually in the package directory using `nix run .#gen-cabal`.
        '';
      };

      scriptQuiet = mkOption {
        type = types.path;
        description = ''
          Same as `script`, but suppress all output.
        '';
      };

      internal.packages = mkOption {
        type = types.attrsOf util.types.strict;
      };

    };
  };

  config = {

    defaultApp = lib.mkDefault config.main;

    hpack = {

      script = util.hpack.gen { verbose = true; };

      scriptQuiet = util.hpack.gen { verbose = false; };

      internal.packages =
        if config.managed.enable
        then util.hpack.conf.packagesWithManaged
        else util.hpack.conf.packages
        ;

    };
  };
}
