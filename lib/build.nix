{
  lib,
  hixModules,
  projectModules,
  extraModules,
}:
let
  util = import ./default.nix { inherit lib; };

  compat = import ./compat.nix { inherit lib; };

  userModules = extraModules ++ map compat.check (lib.toList projectModules);

  allModules = hixModules ++ userModules;

  hixlib = config:
  let util = import ../lib/with-config.nix { inherit config lib; };
  in util;

  moduleArgs = evaledModules: {config, ...}: {
    _module.args = {
      util = hixlib config // { inherit allModules evaledModules; };
    };
  };

  finalModules = system: evaled: allModules ++ [(moduleArgs evaled) { inherit system; }];

  # TODO maybe this could include gen-overrides so the default systems may depend on it
  onlySystemsConfig = m:
  if lib.isFunction m
  then a@{util, ...}: onlySystemsConfig (m a)
  else
  lib.optionalAttrs (m ? systems) { inherit (m) systems; } //
  lib.optionalAttrs (m ? config && m.config ? systems) { config = { inherit (m.config) systems; }; }
  ;

  # TODO should this include extraModules?
  bootConfig = util.evalConfig ([(import ../modules/systems.nix)] ++ map onlySystemsConfig projectModules);

  evalSystem = system: let
    evaled = util.evalModules (finalModules system evaled);
  in evaled.config;

  oneSystem = system: (evalSystem system).output.final;

  dummySystem = evalSystem "x86_64-linux";

  allSystems = util.flake-utils.eachSystem bootConfig.systems oneSystem;

in allSystems // { overrides = dummySystem.exportedOverrides; lib.overrides = dummySystem.exportedOverrides; }
