{ lib, config, withModules, overrides, ... }:
with builtins;
with lib;
with types;
let
  inherit (config.devGhc) pkgs;

  ghcidLib = import ../lib/ghcid/default.nix { inherit lib config withModules; };
in {
  options.shell = {

    hls = {

      package = mkOption {
        description = "The package for HLS.";
        type = package;
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

    package = mkDefault (
      if config.ghcid.easy-hls
      then config.inputs.easy-hls.defaultPackage.${config.system}
      else if config.shell.hls.vanilla
      then config.devGhc.vanillaGhc.haskell-language-server
      else import ../lib/hls.nix { inherit config overrides; }
    );

    app = mkDefault (pkgs.writeScript "hls" "nix develop -c haskell-language-server");

  };
}
