{ lib, modules, }:
with lib;
let
  flake-utils = import (builtins.fetchTarball {
    url = "https://github.com/numtide/flake-utils/archive/refs/tags/v1.0.0.tar.gz";
    sha256 = "0hynd4rbkbplxzl2a8wb3r8z0h17z2alhhdsam78g3vgzpzg0d43";
  });
  hixlib = import ../lib/default.nix { inherit lib; };

  build = config:
  let
    oneSystem = system:
    hixlib.withModules config [{ inherit system; } (import ./system.nix)] (config: config.systemOutputs);

    allSystems = flake-utils.eachSystem config.systems oneSystem;

  in allSystems // { overrides = config.exportedOverrides; };

in
  hixlib.withModules {} modules build
