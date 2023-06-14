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
      description = mdDoc ''
      The overrides used for this package set – see [](#ghc) for an explanation.

      This option is set by environments (see [](#envs)), but GHC modules can be used outside of environments, so this
      might be set by the user.
      '';
      default = [];
    };

    nixpkgs = mkOption {
      type = util.types.nixpkgs;
      description = mdDoc ''
      The path to a nixpkgs source tree, used as the basis for the package set.

      This can be a flake input or a regular type of path, like the result of `fetchGit`.
      '';
    };

    nixpkgsOptions = mkOption {
      type = attrsOf unspecified;
      description = mdDoc "Additional options to pass to nixpkgs when importing.";
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
      description = mdDoc "Additional nixpkgs overlays.";
    };

    vanillaGhc = mkOption {
      type = util.types.ghc;
      description = mdDoc "The package set without overrides.";
      readOnly = true;
    };

    ghc = mkOption {
      type = util.types.ghc;
      description = mdDoc "The package set with overrides.";
      readOnly = true;
    };

    version = mkOption {
      description = mdDoc "The GHC version as a canonical string, like `9.2.5`, for use in conditions.";
      type = str;
      readOnly = true;
    };

  };

  config = {
    compiler = util.unlessDev config global.envs.dev.ghc.compiler;
    overlays = util.unlessDev config global.envs.dev.ghc.overlays;
    nixpkgs = util.unlessDev config global.envs.dev.ghc.nixpkgs;
    nixpkgsOptions = util.unlessDev config global.envs.dev.ghc.nixpkgsOptions;

    pkgs = let
      go = util.ghcOverlay {
        inherit (config) pkgs compiler name overrides;
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
