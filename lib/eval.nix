{
  lib,
  hixModules,
  projectModules,
  extraModules,
}:
let
  libBase = import ./default.nix { inherit lib; };

  userModules = extraModules ++ lib.toList projectModules;

  allModules = hixModules ++ userModules;

  moduleArgs = evaledModules:
  libBase.utilModule { inherit allModules evaledModules; };

  finalModules = system: evaled: allModules ++ [(moduleArgs evaled) { inherit system; }];

  # TODO maybe this could include gen-overrides so the default systems may depend on it
  #
  # TODO we can use `lib.mirrorFunctionArgs` for the wrapper
  onlySystemsConfig = m:
  if lib.isFunction m
  then a@{util, pkgs, build, outputs, project, internal, ...}: onlySystemsConfig (m a)
  else
  lib.optionalAttrs (m ? systems) { inherit (m) systems; } //
  lib.optionalAttrs (m ? config && m.config ? systems) { config = { inherit (m.config) systems; }; }
  ;

  # TODO should this include extraModules?
  bootConfig = libBase.evalConfig ([(import ../modules/systems.nix)] ++ map onlySystemsConfig projectModules);

  evalSystem = system: let
    evaled = libBase.evalModules (finalModules system evaled);
  in evaled.config;

  oneSystem = system: (evalSystem system).output.final;

  dummySystem = evalSystem "x86_64-linux";

  allSystems = libBase.flake-utils.eachSystem bootConfig.systems oneSystem;

in allSystems // { overrides = dummySystem.exportedOverrides; lib.overrides = dummySystem.exportedOverrides; }
