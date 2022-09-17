{ config, lib, util, ... }:
with lib;
let

  parents = modules: {
    options.internal.parents = mkOption { type = lib.types.unspecified; };
    config.internal.parents = modules;
  };

  modulesRaw = config: extra:
  let
    current = attrByPath ["internal" "parents"] [] config ++ extra;
    newModules = evalModules { modules = current ++ [(parents current)]; };
  in newModules;

  withModules = config: extra: f:
  f (modulesRaw config extra).config;

  paramApp = import ./param-app.nix { inherit config lib util; };

  types = import ./types.nix { inherit lib; };

in {
  inherit
  modulesRaw
  withModules
  paramApp
  types
  ;
}
