{util}:
{lib, config, ...}: let

  inherit (util) internal;
  inherit (lib) types;

  toolchain = config.toolchain;

  inherit (toolchain.pkgs.__hix) packages;

  # FIXME make sure to test the performance in the end, by running the test suite in the parent commit for baseline
  # times.

  # FIXME Push the package built by the `ghc-build` test to cachix

  # FIXME update docs

  # FIXME add missing eval warning replacement instructions, like `vanillaGhc` -> `env.toolchain.vanilla`

  docLink = "You can find more information about customizing compilers and package sets at [https://hix.how#ghc].";

  option = args: internal.modules.deprecatedOption ({
    key = "env.ghc";
    extra = ''

    ${docLink}'';
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
      description = ''
      Backwards-compat alias for `toolchain.tag`
      '';
      type = types.str;
    };

    pkgs = option {
      description = ''
      Backwards-compat alias for `toolchain.pkgs`
      '';
      type = types.pkgs;
    };

    overrides = option {
      description = ''
      Backwards-compat alias for `toolchain.overrides`
      '';
      type = util.types.cabalOverrides;
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
      replacement = "package-sets.*.cross";
    };

    overlays = option {
      type = types.listOf util.types.overlay;
      replacement = "nixpkgs.*.overlays";
    };

    vanillaGhc = option {
      type = util.types.haskellPackages;
    };

    ghc = option {
      type = util.types.haskellPackages;
    };

    version = option {
      type = types.str;
    };

    gen-overrides = option {
      type = types.bool;
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
