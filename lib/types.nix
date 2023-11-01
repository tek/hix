{ config, lib, }:
with lib;
with types;

let

  util = import ./default.nix { inherit lib; };

  conditionModule = {
    options = {

      type = mkOption {
        description = mdDoc "Name of an attribute of [](#opt-general-conditions).";
        type = str;
      };

      args = mkOption {
        description = mdDoc "Arguments for the condition handler in [](#opt-general-conditions).";
        type = attrs;
        default = {};
      };

    };
  };

  conditionHandlerModule = {
    options = {

      render = mkOption {
        description = mdDoc "Rendered form for Cabal files.";
        type = str;
      };

      satisfied = mkOption {
        description = mdDoc "Whether the condition is satisfied for derivation dependencies.";
        type = functionTo bool;
        default = _: true;
      };

    };
  };

  verbatimCondition = value: { type = "verbatim"; args = { inherit value; }; };

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

      condition = mkOption {
        description = mdDoc "";
        type = nullOr (coercedTo str verbatimCondition (submodule conditionModule));
        default = null;
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

  cabalDep =
    coercedTo str util.version.normalize (submodule cabalDepModule);

  componentSort = enum ["library" "executable" "test" "benchmark"];

  env = mkOptionType {
    name = "env-ref";
    description = "name of an environment defined in config.envs";
    descriptionClass = "noun";
    check = a: isString a && hasAttr a config.envs;
    merge = mergeOneOption;
  };

  strict = mkOptionType {
    name = "strict";
    description = "computed value safe for evaluation";
    descriptionClass = "noun";
  };

  conditionHandler = submodule conditionHandlerModule;

}
