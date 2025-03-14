{ global, util }:
{ lib, config, ... }:
with lib;
{
  options = with types; {
    name = mkOption {
      type = str;
      description = "A unique identifier of the package set.";
    };

    compiler = mkOption {
      type = str;
      description = "The attribute name for a GHC version in the set `haskell.packages`.";
    };

    overrides = mkOption {
      type = util.types.cabalOverrides;
      description = ''
      The overrides used for this package set – see [](#ghc) for an explanation.

      This option is set by environments (see [](#envs)), but GHC modules can be used outside of environments, so this
      might be set by the user.
      '';
      default = [];
    };

    nixpkgs = mkOption {
      type = util.types.nixpkgs;
      description = ''
      The path to a nixpkgs source tree, used as the basis for the package set.

      This can be a flake input or a regular type of path, like the result of `fetchGit`.
      '';
    };

    nixpkgsOptions = mkOption {
      type = attrsOf unspecified;
      description = "Additional options to pass to nixpkgs when importing.";
    };

    pkgs = mkOption {
      type = util.types.pkgs;
      description = "The nixpkgs set used for this GHC.";
    };

    crossPkgs = mkOption {
      type = util.types.pkgs;
      description = ''
      This option can be used to override the pkgs set used for the Haskell package set, for example an element of
      `pkgsCross`: `envs.dev.ghc.crossPkgs = config.envs.dev.ghc.pkgs.pkgsCross.musl64`
      '';
    };

    overlays = mkOption {
      type = listOf util.types.overlay;
      description = "Additional nixpkgs overlays.";
    };

    vanillaGhc = mkOption {
      type = util.types.ghc;
      description = "The package set without overrides.";
      readOnly = true;
    };

    ghc = mkOption {
      type = util.types.ghc;
      description = "The package set with overrides.";
      readOnly = true;
    };

    version = mkOption {
      description = "The GHC version as a canonical string, like `9.2.5`, for use in conditions.";
      type = str;
      readOnly = true;
    };

    gen-overrides = mkOption {
      description = ''
      Allow this GHC to use pregenerated overrides.
      Has no effect when [](#opt-general-gen-overrides.enable) is `false`.

      Disabled by default, but enabled for GHCs that are defined in an environment.
      '';
      type = bool;
      default = false;
    };

  };

  config = {
    compiler = util.unlessDev config global.envs.dev.ghc.compiler;
    overlays = util.unlessDev config global.envs.dev.ghc.overlays;
    nixpkgs = util.unlessDev config global.envs.dev.ghc.nixpkgs;
    nixpkgsOptions = util.unlessDev config global.envs.dev.ghc.nixpkgsOptions;

    pkgs = let
      go = util.ghcOverlay { ghc = config; };
      options = recursiveUpdate {
        inherit (global) system;
        overlays = [go] ++ config.overlays;
        config.allowUnfree = true;
      } config.nixpkgsOptions;
    in import config.nixpkgs options;

    crossPkgs = mkDefault config.pkgs;

    vanillaGhc = mkDefault (config.crossPkgs.haskell.packages.${config.compiler});

    ghc = config.crossPkgs.hixPackages;

    version = config.ghc.ghc.version;
  };
}
