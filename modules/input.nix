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
      description = "This option is set dynamically for each configured system.";
      type = types.str;
    };

    inputs = genAttrs repos input;

    ghcNixpkgs = mkOption {
      description = "An attrset mapping GHC versions to the nixpkgs inputs used to build them.";
      type = types.unspecified;
    };
  };

  config = {
    ghcNixpkgs = {
      ghc884 = mkDefault config.inputs.nixpkgs_ghc884;
      ghc8107 = mkDefault config.inputs.nixpkgs_ghc8107;
      ghc902 = mkDefault config.inputs.nixpkgs_ghc902;
      ghc921 = mkDefault config.inputs.nixpkgs_ghc921;
    };
  };
}
