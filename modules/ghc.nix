{ global, util }:
{ lib, config, ... }:
with lib;
{
  options = with types; {
    name = mkOption {
      type = str;
      description = mdDoc "An identifier used for describing the package set.";
      default = "unnamed";
    };

    compiler = mkOption {
      type = str;
      description = mdDoc "The attribute name for a GHC version in the set `haskell.packages`.";
    };

    overrides = mkOption {
      type = util.types.cabalOverrides;
      description = mdDoc "The overrides used for this package set.";
      default = [];
    };

    nixpkgs = mkOption {
      type = util.types.nixpkgs;
      description = mdDoc "The flake input pointing to a nixpkgs commit used as the basis for the package set.";
    };

    nixpkgsOptions = mkOption {
      type = attrsOf unspecified;
      description = mdDoc "Additional options to pass to nixpkgs when importing.";
      default = {};
    };

    pkgs = mkOption {
      type = util.types.pkgs;
      description = mdDoc "The nixpkgs set used for this GHC.";
    };

    crossPkgs = mkOption {
      type = util.types.pkgs;
      description = mdDoc ''
      This option can be used to override the pkgs set used for the Haskell package set, for example an element of
      `pkgsCross`: `envs.dev.ghc.crossPkgs = config.envs.dev.ghc.pkgs.pkgsCross.musl64`
      '';
    };

    overlays = mkOption {
      type = listOf util.types.overlay;
      default = [];
      description = mdDoc "Additional nixpkgs overlays.";
    };

    vanillaGhc = mkOption {
      type = util.types.ghc;
      description = mdDoc "The package set without overrides.";
    };

    ghc = mkOption {
      type = util.types.ghc;
      description = mdDoc "The package set with overrides.";
    };

    version = mkOption {
      description = "The GHC version.";
      type = str;
      readOnly = true;
    };

  };

  config = {
    compiler = mkDefault global.mainCompiler;

    nixpkgs = mkDefault global.inputs.nixpkgs;

    pkgs = let
      go = util.ghcOverlay {
        inherit (config) compiler name overrides;
        inherit (config.nixpkgs) rev;
      };
      options = recursiveUpdate {
        inherit (global) system;
        overlays = [go] ++ config.overlays;
        config.allowUnfree = true;
      } config.nixpkgsOptions;
    in import config.nixpkgs options;

    crossPkgs = mkDefault config.pkgs;

    vanillaGhc = mkDefault ((import config.nixpkgs { inherit (global) system; }).haskell.packages.${config.compiler});

    ghc = config.crossPkgs.hixPackages;

    version = config.ghc.ghc.version;
  };
}
