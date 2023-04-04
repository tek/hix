{ inputs, projectModules, }:
let
  lib = inputs.nixpkgs.lib;

  inputsConfig = {
    config.inputs = {
      inherit (inputs)
      nixpkgs
      nixpkgs_ghc943
      nixpkgs_ghc925
      nixpkgs_ghc902
      nixpkgs_ghc8107
      nixpkgs_ghc884
      flake-utils
      thax
      ;
    };
  };

  hixlibc = config:
  let util = import ../lib/with-config.nix { inherit config lib util; };
  in util;

  helpers = { config, ... }: {
    _module.args = { util = hixlibc config // { modules = modules; }; };
  };

  modules = projectModules ++ [
    ./input.nix
    ./basic.nix
    ./hpack.nix
    ./output.nix
    ./shell.nix
    ./services.nix
    ./envs.nix
    ./overrides.nix
    ./commands.nix
    ./ghci.nix
    ./hackage.nix
    inputsConfig
    helpers
  ];

in modules
