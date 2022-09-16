{ lib, config, withModules, overrides, ghcOverlay, ... }:
with builtins;
with lib;
with types;
let
  inherit (config.devGhc) pkgs;

  global = config;

  ghcModule = import ./ghc.nix { inherit global ghcOverlay; };

  ghcidLib = import ../lib/ghcid/default.nix { inherit lib config withModules; };
in {
  options.shell = {

    hls = {

      package = mkOption {
        description = "The package for HLS.";
        type = package;
      };

      ghc = mkOption {
        description = "The GHC config used for HLS, defaulting to the dev GHC without overrides.";
        type = submodule ghcModule;
      };

      vanilla = mkOption {
        description = "If true, use the derivation from nixpkgs. If false, build a custom package from Hackage.";
        type = bool;
        default = true;
      };

      app = mkOption {
        description = "The flake app generated for HLS.";
        type = unspecified;
      };

      overlays = mkOption {
        description = "Overlays to apply to the nixpkgs used for HLS.";
        type = unspecified;
        default = [];
      };

    };

    ghcid = {

      enable = mkOption {
        type = bool;
        description = ''
          Whether to include <literal>ghcid</literal> in the shell inputs.
          Setting this to <literal>false</literal> will prevent the test apps from working.
        '';
        default = true;
      };

    };

  };

  config.shell.hls = {

    ghc = {
      name = "hls";
      compiler = config.devGhc.compiler;
      nixpkgs = config.devGhc.nixpkgs;
      nixpkgsOptions = config.devGhc.nixpkgsOptions;
      overrideKeys = [];
      overlays = config.devGhc.overlays ++ traceVal config.shell.hls.overlays;
    };

    package = mkDefault (
      if config.ghcid.easy-hls
      then config.inputs.easy-hls.defaultPackage.${config.system}
      else if config.shell.hls.vanilla
      then config.shell.hls.ghc.ghc.haskell-language-server
      else import ../lib/hls.nix { inherit config overrides; }
    );

    app = mkDefault (pkgs.writeScript "hls" "nix develop -c haskell-language-server");

  };
}
