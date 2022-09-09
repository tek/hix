{ global, ghcOverlay }:
{ lib, config, ... }:
with lib;
{
  options = with types; {
    name = mkOption {
      type = str;
      description = "An identifier used for describing the package set.";
      default = "unnamed";
    };

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

    nixpkgsOptions = mkOption {
      type = attrsOf unspecified;
      description = "Additional options to pass to nixpkgs when importing.";
      default = {};
    };

    pkgs = mkOption {
      type = unspecified;
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

    pkgs = let
      go = ghcOverlay {
        inherit (global.internal) overrides;
        inherit (config) compiler overrideKeys;
        inherit (config.nixpkgs) rev;
        inherit (config) name;
      };
      options = recursiveUpdate {
        inherit (global) system;
        overlays = [go] ++ config.overlays;
        config.allowUnfree = true;
      } config.nixpkgsOptions;
    in import config.nixpkgs options;

    ghc = config.pkgs.hixPackages;

    vanillaGhc = mkDefault ((import config.nixpkgs { inherit (global) system; }).haskell.packages.${config.compiler});
  };
}
