{ config, lib, util, ... }:
let
  inherit (lib)
  types
  mkOption
  literalExpression
  mkOptionType
  mergeOneOption
  ;

  inherit (types)
  str
  nullOr
  int
  bool
  either
  listOf
  submodule
  ;

  fileModule = name: import ../modules/${name}.nix { inherit util; };

  fileSubmodule = name: types.submodule (fileModule name);

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

  nestedPackages = types.lazyAttrsOf (either types.package nestedPackages);

  # Not checking that `program` is a string because it would evaluate it
  checkFlakeApp = a:
    lib.isAttrs a &&
    a ? type &&
    lib.isString a.type &&
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
    check = a: lib.isFunction a || (lib.isList a && lib.all lib.isFunction a);
    merge = _: defs: util.overridesVia desc (lib.concatLists (map (a: lib.toList a.value) defs));
  };

  passwordModule = {

    options = {

      type = lib.mkOption {
        description = ''
        How to interpret the password value.
        * `plain` means the password is literally in-place
        * `env-var` means the value is the name of an env variable containing the password
        * `exec` means the value is the path to an executable that prints the password
        '';
        type = types.enum ["plain" "env-var" "exec"];
      };

      value = lib.mkOption {
        description = "The password or a reference to it.";
        type = types.str;
      };

    };

  };

in {
  inherit nestedPackages flakeApp cabalOverridesVia;

  ref = {

    nixpkgs = mkOptionType {
      name = "ref.nixpkgs";
      description = "name of a nixpkgs configuration in config.nixpkgs.";
      descriptionClass = "noun";
      check = a: lib.isString a && lib.hasAttr a config.nixpkgs;
      merge = mergeOneOption;
    };

    compiler = mkOptionType {
      name = "ref.compiler";
      description = "name of a compiler defined in config.compilers";
      descriptionClass = "noun";
      check = a: lib.isString a && lib.hasAttr a config.compilers;
      merge = mergeOneOption;
    };

    package-set = mkOptionType {
      name = "ref.package-set";
      description = "name of a package set defined in config.package-sets";
      check = a: lib.isString a && lib.hasAttr a config.package-sets;
      merge = mergeOneOption;
    };

  };

  nixpkgs = fileModule "nixpkgs";

  compiler = fileModule "compiler";

  package-set = fileModule "package-set";

  toolchain = fileSubmodule "toolchain";

  haskellPackages = mkOptionType {
    name = "haskellPackages";
    description = "Haskell package set";
    check = a: lib.isAttrs a;
    merge = mergeOneOption;
  };

  overlay = types.functionTo (types.functionTo types.raw) // {
    name = "overlay";
    description = "nixpkgs overlay";
    descriptionClass = "noun";
  };

  cabalOverrides = cabalOverridesVia null;

  cabalDep = either str (submodule cabalDepModule);

  bounds = submodule boundsModule;

  componentSort = types.enum ["library" "executable" "test" "benchmark"];

  localPackage = mkOptionType {
    name = "local-package";
    description = "name of a package defined in config.packages";
    descriptionClass = "noun";
    check = a: lib.isString a && lib.hasAttr a config.packages;
    merge = mergeOneOption;
  };

  env = mkOptionType {
    name = "env-ref";
    description = "name of an environment defined in config.envs";
    descriptionClass = "noun";
    check = a: lib.isString a && lib.hasAttr a config.envs;
    merge = mergeOneOption;
  };

  hpackDep = mkOptionType {
    name = "hpack-dep";
    description = "package dependency in HPack format";
    descriptionClass = "noun";
    check = a: lib.isString a || lib.isAttrs a;
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
    check = lib.isBool;
    merge = _: defs: lib.all lib.id (map (a: a.value) defs);
  };

  password = types.either types.str (submodule passwordModule);

}
