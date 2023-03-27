{ lib, modules, }:
with lib;
let
  hixlib = import ../lib/default.nix { inherit lib; };

  build = config:
  let
    oneSystem = system:
    hixlib.withModules config [{ inherit system; } (import ./system.nix)] (config: config.systemOutputs);

    allSystems = config.inputs.flake-utils.lib.eachSystem config.output.systems oneSystem;

  in allSystems // { overrides = config.exportedOverrides; };

in
  hixlib.withModules {} modules build
