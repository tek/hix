{ config, lib, }:
with lib;
with types;

let

  util = import ./default.nix { inherit lib; };

  cabalDepModule = {
    options = {

      name = mkOption {
        description = mdDoc "Name of the package.";
        type = str;
      };

      version = mkOption {
        description = mdDoc "Version of the package.";
        type = nullOr (either int str);
        default = null;
      };

      mixin = mkOption {
        description = mdDoc "List of Cabal mixins that allow renaming and hiding modules.";
        type = listOf str;
        default = [];
        example = literalExpression ''
          [
            "(IncipitBase as Prelude)"
            "hiding (IncipitBase)"
          ]
        '';
      };

      local = mkOption {
        description = mdDoc ''
        Whether this package is part of this build.
        When this is set, the version is used when managed dependencies are enabled and this dep has no managed version.
        '';
        type = bool;
        default = false;
      };

    };
  };

  boundsModule = {
    options = {

      lower = mkOption {
        description = mdDoc "The lower bound, inclusive.";
        type = nullOr str;
        default = null;
        example = literalExpression ''"1.4.8"'';
      };

      upper = mkOption {
        description = mdDoc "The upper bound, exclusive.";
        type = nullOr str;
        default = null;
        example = literalExpression ''"1.7"'';
      };

    };
  };

  nestedPackages = lazyAttrsOf (either package nestedPackages);

  flakeApp = mkOptionType {
    name = "flake-app";
    description = "A flake output of type 'app'";
    descriptionClass = "noun";
    check = a: isAttrs a && a ? type && a ? program;
    merge = mergeOneOption;
  };

  nestedFlakeApps = lazyAttrsOf (either flakeApp nestedFlakeApps);

  cabalOverridesVia = desc: mkOptionType {
    name = "cabal-overrides";
    description = "A Haskell package override function specified in the Hix DSL";
    descriptionClass = "noun";
    check = a: isFunction a || (isList a && all isFunction a);
    merge = _: defs: util.overridesVia desc (concatLists (map (a: toList a.value) defs));
  };

in {
  inherit nestedPackages flakeApp nestedFlakeApps cabalOverridesVia;

  nixpkgs = mkOptionType {
    name = "nixpkgs";
    description = "A nixpkgs snapshot";
    merge = mergeOneOption;
  };

  pkgs = mkOptionType {
    name = "pkgs";
    description = "A nixpkgs attrset";
    merge = mergeOneOption;
  };

  overlay = mkOptionType {
    name = "overlay";
    description = "An overlay";
    merge = mergeOneOption;
  };

  cabalOverrides = cabalOverridesVia null;

  ghc = mkOptionType {
    name = "ghc";
    description = "A Haskell package set";
    merge = mergeOneOption;
  };

  cabalDep = either str (submodule cabalDepModule);

  bounds = submodule boundsModule;

  componentSort = enum ["library" "executable" "test" "benchmark"];

  localPackage = mkOptionType {
    name = "local-package";
    description = "name of a package defined in config.packages";
    descriptionClass = "noun";
    check = a: isString a && hasAttr a config.packages;
    merge = mergeOneOption;
  };

  env = mkOptionType {
    name = "env-ref";
    description = "name of an environment defined in config.envs";
    descriptionClass = "noun";
    check = a: isString a && hasAttr a config.envs;
    merge = mergeOneOption;
  };

  hpackDep = mkOptionType {
    name = "hpack-dep";
    description = "package dependency in HPack format";
    descriptionClass = "noun";
    check = a: isString a || isAttrs a;
    merge = mergeOneOption;
  };

  strict = mkOptionType {
    name = "strict";
    description = "computed value safe for evaluation";
    descriptionClass = "noun";
  };

  multiEnable = mkOptionType {
    name = "multi-enable";
    description = "boolean flag that uses conjunction for merging";
    descriptionClass = "noun";
    check = isBool;
    merge = _: defs: all id (map (a: a.value) defs);
  };

}
