{util}:
{lib, config, ...}: let

  inherit (util) internal;
  inherit (lib) types;

  toolchain = config.toolchain;

  inherit (toolchain.pkgs.__hix) packages;

  docLink = "You can find more information about customizing compilers and package sets at [https://hix.how#ghc].";

  evalWarning = {key, loc, replacement ? null}:
    internal.warn.deprecatedOptionReadOnly
    {
      inherit key replacement;
      option = lib.showOption loc;
      extra = ''

      ${docLink}'';
    };

  deprecatedOptionDefined = loc:
  ''
  The option '${lib.showOption loc}' is deprecated.
  ${docLink}
  '';

  deprecated = {type, replacement ? null}: type // {
    name = "deprecated ${type.name}";
    merge = loc: defs:
      if lib.length defs == 1
      then evalWarning { key = "env.ghc"; inherit loc replacement; } (lib.head defs).value
      else throw (deprecatedOptionDefined loc)
      ;
  };

  deprecatedOption = {
    type,
    replacement ? null,
    description ? "Deprecated.",
  }: lib.mkOption {
    inherit description;
    type = deprecated { inherit type replacement; };
  };

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

    compiler = deprecatedOption {
      description = ''
      Backwards-compat alias for `toolchain.tag`
      '';
      type = types.str;
    };

    pkgs = deprecatedOption {
      description = ''
      Backwards-compat alias for `toolchain.pkgs`
      '';
      type = types.pkgs;
    };

    overrides = deprecatedOption {
      description = ''
      Backwards-compat alias for `toolchain.overrides`
      '';
      type = util.types.cabalOverrides;
    };

    nixpkgs = deprecatedOption {
      type = types.anything;
      replacement = "nixpkgs.*.source";
    };

    nixpkgsOptions = deprecatedOption {
      type = types.anything;
      replacement = "nixpkgs.*.config";
    };

    crossPkgs = deprecatedOption {
      type = types.pkgs;
      replacement = "package-sets.*.cross";
    };

    overlays = deprecatedOption {
      type = types.listOf util.types.overlay;
      replacement = "nixpkgs.*.overlays";
    };

    vanillaGhc = deprecatedOption {
      type = util.types.ghc;
    };

    ghc = deprecatedOption {
      type = util.types.haskellPackages;
    };

    version = deprecatedOption {
      type = types.str;
    };

    gen-overrides = deprecatedOption {
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
