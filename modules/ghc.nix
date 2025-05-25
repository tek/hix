{util}:
{lib, config, ...}: let

  inherit (util) internal;
  inherit (lib) types;

  toolchain = config.toolchain;

  inherit (toolchain.pkgs.__hix) packages;

  docLink = "You can find more information about customizing compilers and package sets at [https://hix.how#ghc].";

  option = args: internal.modules.deprecatedOption ({
    key = "env.ghc";
    extra = "\n${docLink}";
  } // args);

in {
  options = {

    toolchain = lib.mkOption {
      description = ''
      Reference to the package set structure of an environment.
      Used for backwards-compat options.
      '';
      type = util.types.toolchain;
      readOnly = true;
    };

    compiler = option {
      type = types.str;
      replacement = "envs.*.toolchain.compiler";
      definitionReplacement = "compilers.*.source";
    };

    pkgs = option {
      type = types.pkgs;
      replacement = "envs.*.toolchain.pkgs";
      definitionReplacement = "compilers.*.nixpkgs";
    };

    overrides = option {
      type = util.types.cabalOverrides;
      replacement = "envs.*.toolchain.overrides";
      definitionReplacement = "package-sets.*.overrides";
    };

    nixpkgs = option {
      type = types.anything;
      replacement = "nixpkgs.*.source";
    };

    nixpkgsOptions = option {
      type = types.anything;
      replacement = "nixpkgs.*.config";
    };

    crossPkgs = option {
      type = types.pkgs;
      replacement = "envs.*.toolchain.pkgs";
      definitionReplacement = "package-sets.*.cross";
    };

    overlays = option {
      type = types.listOf util.types.overlay;
      replacement = "nixpkgs.*.overlays";
    };

    vanillaGhc = option {
      type = util.types.haskellPackages;
      replacement = "envs.*.toolchain.vanilla";
    };

    ghc = option {
      type = util.types.haskellPackages;
      replacement = "envs.*.toolchain.packages";
    };

    version = option {
      type = types.str;
      replacement = "envs.*.toolchain.version";
    };

    gen-overrides = option {
      type = types.bool;
      replacement = "package-sets.*.gen-overrides";
    };

  };

  config = {

    compiler = toolchain.tag;

    pkgs = toolchain.pkgs;

    overrides = toolchain.overrides;

    nixpkgs = {};

    nixpkgsOptions = {};

    vanillaGhc = toolchain.vanilla;

    ghc = packages;

    version = packages.ghc.version;

    gen-overrides = toolchain.conf.gen-overrides;

  };

}
