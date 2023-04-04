{ lib, config, util, ... }:
with builtins;
with lib;
with types;
let
  inherit (config.envs.dev.ghc) pkgs;

  global = config;

  cfg = config.shell.hls;

  ghcModule = import ./ghc.nix { inherit global util; };

in {

  options.shell = {

    ghcid = {

      enable = mkOption {
        type = bool;
        description = mdDoc ''
          Whether to include `ghcid` in the shell inputs.
          Setting this to `false` will prevent the test apps from working.
        '';
        default = true;
      };

      vanilla = mkOption {
        type = bool;
        description = mdDoc ''
          Whether `ghcid` should be taken from the vanilla package set without any of the overrides.
        '';
        default = true;
      };

    };

  };
}
