{util}:
{config, lib, ...}: let

  inherit (lib) types;

in {

  options = {

    version = lib.mkOption {
      description = ''
      The GHC version built by the custom config.
      If [](#opt-ghc-build-rev) or [](#opt-ghc-build-src) are not specified, the release sources for that version
      are used.
      In any case, the version determines other aspects in nixpkgs' builder, like patches.
      '';
      type = types.nullOr types.str;
      default = null;
    };

    hash = lib.mkOption {
      description = "The hash of the source tree. Will be printed by Nix for you to copy.";
      type = types.str;
      default = "";
    };

    src = lib.mkOption {
      description = "The source tree for the build.";
      type = types.nullOr types.path;
      default = null;
    };

    rev = lib.mkOption {
      description = "The commit hash in the GHC repo used for the build.";
      type = types.nullOr types.str;
      default = null;
    };

    url = lib.mkOption {
      description = "A URL to the source archive for the build.";
      type = types.nullOr types.str;
      default = null;
    };

    postFetch = lib.mkOption {
      description = "";
      type = types.nullOr types.str;
      default = null;
    };

    flavour = lib.mkOption {
      description = "The Hadrian flavour used to build this GHC.";
      type = types.nullOr types.str;
      default = null;
    };

    bootCompiler = lib.mkOption {
      description = ''
      The name of the package set in `haskell.packages` used to build GHC.
      If you have overridden Hix' `nixpkgs` input or building a sufficiently nonstandard configuration, you may have
      to adjust this.
      '';
      type = types.str;
      default = "ghc963Binary";
    };

    builderArgs = lib.mkOption {
      description = "Overrides for the arguments passed to nixpkgs' Hadrian builder.";
      type = types.attrsOf types.anything;
      default = {};
    };

    compilerConfig = lib.mkOption {
      description = ''
      Basic package overrides for the GHC package set.
      These handle a GHC version's specific requirements, which mostly consists of removing derivations for core
      libraries like `text` that are distributed with GHC, as well as choosing compatible versions for important
      packages.

      For known GHC versions, this defaults to the files shipped with nixpkgs, at paths like:
      `pkgs/development/haskell-modules/configuration-ghc-9.10.x.nix`
      '';
      type = types.nullOr (types.functionTo (types.functionTo types.raw));
      default = null;
    };

  };

  config = {

    bootCompiler = lib.mkDefault (
      if config.version != null && lib.versionAtLeast config.version "9.12"
      then "ghc9101"
      else "ghc963Binary"
    );

  };

}
