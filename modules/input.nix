{ config, lib, util, ... }:
with lib;
let

  nixpkgsRepos = [
    "nixpkgs"
    "nixpkgs_ghc942"
    "nixpkgs_ghc924"
    "nixpkgs_ghc902"
    "nixpkgs_ghc8107"
    "nixpkgs_ghc884"
  ];

  repos = [
    "flake-utils"
    "obelisk"
    "thax"
  ];

  input = name: mkOption {
    type = types.unspecified;
    description = "The ${name} repository.";
  };

  nixpkgsInput = name: mkOption {
    type = util.types.nixpkgs;
    description = "The ${name} repository.";
  };

in {
  options = {
    system = mkOption {
      description = "This option is set dynamically for each configured system.";
      type = types.str;
    };

    inputs = genAttrs nixpkgsRepos nixpkgsInput // genAttrs repos input;

    input.ghcNixpkgs = mkOption {
      description = "An attrset mapping GHC versions to the nixpkgs inputs used to build them.";
      type = types.lazyAttrsOf util.types.nixpkgs;
    };
  };

  config = {
    input.ghcNixpkgs = {
      ghc884 = mkDefault config.inputs.nixpkgs_ghc884;
      ghc8107 = mkDefault config.inputs.nixpkgs_ghc8107;
      ghc902 = mkDefault config.inputs.nixpkgs_ghc902;
      ghc924 = mkDefault config.inputs.nixpkgs_ghc924;
      ghc942 = mkDefault config.inputs.nixpkgs_ghc942;
    };
  };
}
