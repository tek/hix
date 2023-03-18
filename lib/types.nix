{ lib, }:
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

    };
  };

in {

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
    merge = mergeOneOption;
  };

  ghc = mkOptionType {
    name = "ghc";
    description = "A Haskell package set";
    merge = mergeOneOption;
  };

  cabalDep = either str (submodule cabalDepModule);

}
