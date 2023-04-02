{ lib, }:
with lib;
with types;

let

  util = import ./default.nix { inherit lib; };

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

    };
  };

  nestedPackages = lazyAttrsOf (either package nestedPackages);

  # TODO concrete type
  flakeApp = lazyAttrsOf str;

  nestedFlakeApps = lazyAttrsOf (either flakeApp nestedFlakeApps);

in {
  inherit nestedPackages flakeApp nestedFlakeApps;

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

  cabalOverrides = mkOptionType {
    name = "cabal-overrides";
    description = "A Haskell package override function specified in the Hix DSL";
    descriptionClass = "noun";
    check = a: isFunction a || (isList a && all isFunction a);
    merge = _: defs: concatLists (map (a: toList a.value) defs);
  };

  ghc = mkOptionType {
    name = "ghc";
    description = "A Haskell package set";
    merge = mergeOneOption;
  };

  cabalDep = either str (submodule cabalDepModule);

}
