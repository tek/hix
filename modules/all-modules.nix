{ inputs, projectModules, }:
let
  lib = inputs.nixpkgs.lib;

  inputsConfig = {
    config.inputs = {
      inherit (inputs)
      nixpkgs
      nixpkgs_ghc942
      nixpkgs_ghc924
      nixpkgs_ghc902
      nixpkgs_ghc8107
      nixpkgs_ghc884
      flake-utils
      obelisk
      thax
      ;
    };
  };

  hixlib = import ../lib/default.nix { inherit lib; };

  helpers = {
    _module.args = hixlib;
  };

in
  projectModules ++ [
    ./input.nix
    ./haskell.nix
    ./hpack.nix
    ./output.nix
    ./ghci.nix
    ./ghcid.nix
    ./shell.nix
    ./hackage.nix
    inputsConfig
    helpers
  ]
