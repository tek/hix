{ lib, config, util, ... }:
with builtins;
with lib;
with types;
let
  inherit (config.envs.dev.ghc) pkgs;

  global = config;

  cfg = config.shell.hls;

  ghcModule = import ./ghc.nix { inherit global util; };

  ghcidLib = import ../lib/ghcid/default.nix { inherit lib config util; };

in {

  options.shell = {

    hls = {

      package = mkOption {
        description = mdDoc "The package for HLS.";
        type = package;
      };

      ghc = mkOption {
        description = mdDoc "The GHC config used for HLS, defaulting to the dev GHC without overrides.";
        type = submodule ghcModule;
      };

      vanilla = mkOption {
        description = mdDoc "If true, use the derivation from nixpkgs. If false, build a custom package from Hackage.";
        type = bool;
        default = true;
      };

      app = mkOption {
        description = mdDoc "The script for a flake app that executes HLS.";
        type = path;
      };

      overlays = mkOption {
        description = mdDoc "Overlays to apply to the nixpkgs used for HLS.";
        type = unspecified;
        default = [];
      };

    };

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

  config.shell.hls = {

    ghc = {
      name = "hls";
      compiler = config.envs.dev.ghc.compiler;
      nixpkgs = config.envs.dev.ghc.nixpkgs;
      nixpkgsOptions = config.envs.dev.ghc.nixpkgsOptions;
      overrideKeys = [];
      overlays = config.envs.dev.ghc.overlays ++ cfg.overlays;
    };

    package = mkDefault cfg.ghc.ghc.haskell-language-server;

    app = mkDefault (pkgs.writeScript "hls" "nix develop -c haskell-language-server");

  };
}
