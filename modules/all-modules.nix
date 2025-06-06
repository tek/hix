{inputs}:
let
  inputsConfig.config.inputs = inputs;

  modules = [
    ./systems.nix
    ./system.nix
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
    ./managed.nix
    ./ui.nix
    ./build-tools.nix
    inputsConfig
  ];

in modules
