{ global, ghcOverlay, util }:
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

    overrides = mkOption {
      type = util.types.cabalOverrides;
      description = "The overrides used for this package set, defaulting to the global overrides.";
    };

    overrideKeys = mkOption {
      type = listOf str;
      description = "The keys of the overrides used for this package set, in increasing order of precedence.";
    };

    nixpkgs = mkOption {
      type = util.types.nixpkgs;
      description = "The flake input pointing to a nixpkgs commit used as the basis for the package set.";
    };

    nixpkgsOptions = mkOption {
      type = attrsOf unspecified;
      description = "Additional options to pass to nixpkgs when importing.";
      default = {};
    };

    pkgs = mkOption {
      type = util.types.pkgs;
    };

    crossPkgs = mkOption {
      type = util.types.pkgs;
      description = ''
      This option can be used to override the pkgs set used for the Haskell package set, for example an element of
      <literal>pkgsCross</literal>: <literal>devGhc.crossPkgs = config.devGhc.pkgs.pkgsCross.musl64</literal>
      '';
    };

    overlays = mkOption {
      type = listOf util.types.overlay;
      default = [];
    };

    vanillaGhc = mkOption {
      type = util.types.ghc;
    };

    ghc = mkOption {
      type = util.types.ghc;
    };
  };

  config = {
    compiler = mkDefault global.mainCompiler;

    overrides = mkDefault global.internal.overrides;

    overrideKeys = mkDefault ["local" "all" config.compiler "dev"];

    nixpkgs = mkDefault global.inputs.nixpkgs;

    pkgs = let
      go = ghcOverlay {
        inherit (config) overrides compiler overrideKeys;
        inherit (config.nixpkgs) rev;
        inherit (config) name;
      };
      options = recursiveUpdate {
        inherit (global) system;
        overlays = [go] ++ config.overlays;
        config.allowUnfree = true;
      } config.nixpkgsOptions;
    in import config.nixpkgs options;

    crossPkgs = mkDefault config.pkgs;

    ghc = config.crossPkgs.hixPackages;

    vanillaGhc = mkDefault ((import config.nixpkgs { inherit (global) system; }).haskell.packages.${config.compiler});
  };
}
