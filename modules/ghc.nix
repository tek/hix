{ global, util }:
{ lib, config, ... }: let

  inherit (lib) types;

in {
  options = {
    name = lib.mkOption {
      type = types.str;
      description = "A unique identifier of the package set.";
    };

    compiler = lib.mkOption {
      type = types.str;
      description = "The attribute name for a GHC version in the set `haskell.packages`.";
    };

    overrides = lib.mkOption {
      type = util.types.cabalOverrides;
      description = ''
      The overrides used for this package set – see [](#ghc) for an explanation.

      This option is set by environments (see [](#envs)), but GHC modules can be used outside of environments, so this
      might be set by the user.
      '';
      default = [];
    };

    nixpkgs = lib.mkOption {
      type = util.types.nixpkgs;
      description = ''
      The path to a nixpkgs source tree, used as the basis for the package set.

      This can be a flake input or a regular type of path, like the result of `fetchGit`.
      '';
    };

    # TODO make sure this is the type that recursively merges attrsets
    nixpkgsOptions = lib.mkOption {
      type = types.attrsOf types.unspecified;
      description = "Additional options to pass to nixpkgs when importing.";
    };

    pkgs = lib.mkOption {
      type = util.types.pkgs;
      description = "The nixpkgs set used for this GHC.";
    };

    crossPkgs = lib.mkOption {
      type = util.types.pkgs;
      description = ''
      This option can be used to override the pkgs set used for the Haskell package set, for example an element of
      `pkgsCross`: `envs.dev.ghc.crossPkgs = config.envs.dev.ghc.pkgs.pkgsCross.musl64`
      '';
    };

    overlays = lib.mkOption {
      type = types.listOf util.types.overlay;
      description = "Additional nixpkgs overlays.";
    };

    vanillaGhc = lib.mkOption {
      type = util.types.ghc;
      description = "The package set without overrides.";
      readOnly = true;
    };

    ghc = lib.mkOption {
      type = util.types.ghc;
      description = "The package set with overrides.";
      readOnly = true;
    };

    version = lib.mkOption {
      description = "The GHC version as a canonical string, like `9.2.5`, for use in conditions.";
      type = types.str;
      readOnly = true;
    };

    gen-overrides = lib.mkOption {
      description = ''
      Allow this GHC to use pregenerated overrides.
      Has no effect when [](#opt-general-gen-overrides.enable) is `false`.

      Disabled by default, but enabled for GHCs that are defined in an environment.
      '';
      type = types.bool;
      default = false;
    };

  };

  config = {
    compiler = util.unlessDev config global.envs.dev.ghc.compiler;
    overlays = util.unlessDev config global.envs.dev.ghc.overlays;
    nixpkgs = util.unlessDev config global.envs.dev.ghc.nixpkgs;
    nixpkgsOptions = util.unlessDev config global.envs.dev.ghc.nixpkgsOptions;

    pkgs = let
      ghcOverlay = util.ghcOverlay { ghc = config; };
      options = lib.recursiveUpdate {
        inherit (global) system;
        overlays = [ghcOverlay] ++ config.overlays;
        config.allowUnfree = true;
      } config.nixpkgsOptions;
    in import config.nixpkgs options;

    crossPkgs = lib.mkDefault config.pkgs;

    vanillaGhc = lib.mkDefault (config.crossPkgs.haskell.packages.${config.compiler});

    ghc = config.crossPkgs.hixPackages;

    version = config.ghc.ghc.version;
  };
}
