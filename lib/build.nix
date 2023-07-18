{ lib, hixModules, projectModules, extraModules, }:
with lib;
let
  util = import ./default.nix { inherit lib; };

  compat = import ./compat.nix { inherit lib; };

  userModules = extraModules ++ map compat.check (toList projectModules);

  allModules = hixModules ++ userModules;

  onlySystemsConfig = m:
  if isFunction m
  then a@{util, ...}: onlySystemsConfig (m a)
  else
  optionalAttrs (m ? systems) { inherit (m) systems; } //
  optionalAttrs (m ? config && m.config ? systems) { config = { inherit (m.config) systems; }; }
  ;

  bootConfig = util.evalModules ([(import ../modules/systems.nix)] ++ map onlySystemsConfig projectModules);

  evalSystem = system: util.evalModules (allModules ++ [{ inherit system; }]);

  oneSystem = system: (evalSystem system).output.final;

  dummySystem = evalSystem "x86_64-linux";

  allSystems = util.flake-utils.eachSystem bootConfig.systems oneSystem;

in allSystems // { overrides = dummySystem.exportedOverrides; lib.overrides = dummySystem.exportedOverrides; }
