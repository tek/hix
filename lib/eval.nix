{
  lib,
  hixModules,
  projectModules,
  extraModules,
}:
let
  libBase = import ./default.nix { inherit lib; };

  addFile = _file: module:
  if lib.isFunction module
  then lib.mirrorFunctionArgs module (args: { inherit _file; } // module args)
  else module
  ;

  canonicalize = file: spec:
  if lib.isFunction spec
  then addFile file spec
  else if builtins.isAttrs spec
  then canonicalize file (_: spec)
  else if lib.isPath spec
  then canonicalize spec (import spec)
  # Let nixpkgs throw errors.
  else spec
  ;

  userModules = extraModules ++ lib.toList projectModules;

  canonicalModules = map (canonicalize "flake.nix") userModules;

  allModules = hixModules ++ canonicalModules;

  moduleArgs = evaledModules:
  libBase.utilModule { inherit allModules evaledModules; };

  finalModules = system: evaled: allModules ++ [(moduleArgs evaled) { inherit system; }];

  # TODO maybe this could include gen-overrides so the default systems may depend on it
  onlySystemsConfig = m:
  if lib.isFunction m
  then lib.mirrorFunctionArgs m (args: onlySystemsConfig (m args))
  # Anything irregular will be ignored by the `?` operator.
  else
  lib.optionalAttrs (m ? systems) { inherit (m) systems; } //
  lib.optionalAttrs (m ? config && m.config ? systems) { config = { inherit (m.config) systems; }; }
  ;

  bootConfig = libBase.evalConfig ([(import ../modules/systems.nix)] ++ map onlySystemsConfig canonicalModules);

  evalSystem = system: let
    evaled = libBase.evalModules (finalModules system evaled);
  in evaled.config;

  oneSystem = system: (evalSystem system).output.final;

  dummySystem = evalSystem "x86_64-linux";

  allSystems = libBase.flake-utils.eachSystem bootConfig.systems oneSystem;

in allSystems // { overrides = dummySystem.exportedOverrides; lib.overrides = dummySystem.exportedOverrides; }
