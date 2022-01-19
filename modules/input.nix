{ lib, ... }:
with lib;
let
  repos = [
    "nixpkgs"
    "nixpkgs_ghc921"
    "nixpkgs_ghc902"
    "nixpkgs_ghc8107"
    "nixpkgs_ghc884"
    "flake-utils"
    "obelisk"
    "thax"
  ];

  input = name: mkOption {
    type = types.unspecified;
    description = "The ${name} repository.";
  };
in {
  options = {
    system = mkOption {
      type = types.str;
    };
    inputs = genAttrs repos input;
  };
}
