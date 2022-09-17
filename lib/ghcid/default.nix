{ lib, config, withModules, }:
with builtins;
with lib;
rec {
  command = import ./command.nix { inherit lib config; };
  shell = import ./shell.nix { inherit lib config command withModules; };
}
