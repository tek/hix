{ inputs, projectModules, }:
let
  lib = inputs.nixpkgs.lib;

  inputsConfig.config.inputs = inputs;

  hixlib = config:
  let util = import ../lib/with-config.nix { inherit config lib util; };
  in util;

  helpers = { config, ... }: {
    _module.args = {
      util = hixlib config // { modules = modules; };
    };
  };

  modules = projectModules ++ [
    ./input.nix
    ./basic.nix
    ./hpack.nix
    ./output.nix
    ./services.nix
    ./envs.nix
    ./overrides.nix
    ./commands.nix
    ./ghci.nix
    ./hackage.nix
    ./cli.nix
    inputsConfig
    helpers
  ];

in modules
