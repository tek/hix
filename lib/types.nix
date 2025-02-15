{ config, lib, util, ... }:
with lib;
with types;

let

  cabalDepModule = {
    options = {

      name = mkOption {
        description = "Name of the package.";
        type = str;
      };

      version = mkOption {
        description = "Version of the package.";
        type = nullOr (either int str);
        default = null;
      };

      mixin = mkOption {
        description = "List of Cabal mixins that allow renaming and hiding modules.";
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
        description = ''
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
        description = "The lower bound, inclusive.";
        type = nullOr str;
        default = null;
        example = literalExpression ''"1.4.8"'';
      };

      upper = mkOption {
        description = "The upper bound, exclusive.";
        type = nullOr str;
        default = null;
        example = literalExpression ''"1.7"'';
      };

    };
  };

  nestedPackages = lazyAttrsOf (either package nestedPackages);

  # Not checking that `program` is a string because it would evaluate it
  checkFlakeApp = a:
    isAttrs a &&
    a ? type &&
    isString a.type &&
    a ? program;

  flakeApp = mkOptionType {
    name = "flake-app";
    description = "flake output of type 'app'";
    descriptionClass = "noun";
    check = checkFlakeApp;
    merge = mergeOneOption;
  };

  cabalOverridesVia = desc: mkOptionType {
    name = "cabal-overrides";
    description = "Haskell package override function specified in the Hix DSL";
    descriptionClass = "noun";
    check = a: isFunction a || (isList a && all isFunction a);
    merge = _: defs: util.overridesVia desc (concatLists (map (a: toList a.value) defs));
  };

in {
  inherit nestedPackages flakeApp cabalOverridesVia app;

  nixpkgs = mkOptionType {
    name = "nixpkgs";
    description = "nixpkgs snapshot";
    merge = mergeOneOption;
  };

  pkgs = mkOptionType {
    name = "pkgs";
    description = "nixpkgs attrset";
    merge = mergeOneOption;
  };

  overlay = mkOptionType {
    name = "overlay";
    description = "overlay";
    merge = mergeOneOption;
  };

  cabalOverrides = cabalOverridesVia null;

  ghc = mkOptionType {
    name = "ghc";
    description = "Haskell package set";
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
