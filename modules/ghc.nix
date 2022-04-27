global:
{ lib, config, ... }:
with lib;
{
  options = with types; {
    compiler = mkOption {
      type = str;
      description = "The attribute name for a GHC version in the set <literal>haskell.packages</literal>.";
    };

    overrideKeys = mkOption {
      type = listOf str;
      description = "The overrides used for this package set, in increasing order of precedence.";
    };

    nixpkgs = mkOption {
      type = unspecified;
      description = "The flake input pointing to a nixpkgs commit used as the basis for the package set.";
    };

    nixpkgsFunc = mkOption {
      type = functionTo unspecified;
    };

    pkgs = mkOption {
      type = unspecified;
    };

    ghcOverlay = mkOption {
      type = functionTo (functionTo unspecified);
    };

    overlays = mkOption {
      type = listOf unspecified;
      default = [];
    };

    vanillaGhc = mkOption {
      type = unspecified;
    };

    ghc = mkOption {
      type = unspecified;
    };
  };

  config = {
    compiler = mkDefault global.mainCompiler;

    overrideKeys = mkDefault ["local" "all" config.compiler "dev"];

    nixpkgs = mkDefault global.inputs.nixpkgs;

    nixpkgsFunc = mkDefault (import config.nixpkgs);

    ghcOverlay = mkDefault (import ../lib/ghc-overlay.nix {
      inherit (global.internal) overrides;
      inherit (config) compiler overrideKeys;
    });

    pkgs = mkDefault (config.nixpkgsFunc {
      inherit (global) system;
      overlays = [config.ghcOverlay] ++ config.overlays;
      config.allowUnfree = true;
    });

    ghc = mkDefault config.pkgs.hixPackages;

    vanillaGhc = mkDefault ((import config.nixpkgs { inherit (global) system; }).haskell.packages.${config.compiler});
  };
}
