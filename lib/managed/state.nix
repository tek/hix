{util}: let
  inherit (util) config lib;

  current = let
    file = "${config.base}/${config.managed.file}";
  in
  { bounds = {}; overrides = {}; resolving = false; } //
  lib.optionalAttrs (config.managed.enable && lib.pathExists file) (import file);

  managedEnvGhc =
    if config.managed.internal.localsInPackageDb
    then util.ghc.packageDbLocal
    else util.ghc.packageDbVanilla
    ;

  cliConfig = let
    package = name: pkg: {
      inherit name;
      inherit (pkg.cabal-config) version;
      deps = map util.version.normalize (lib.concatMap (c: c.dependencies) (lib.attrValues pkg.internal.componentsSet));
    };
  in {
    packages = lib.mapAttrs package config.packages;
    state = util.managed.state.current;
    lower = {
      inherit (config.managed.lower) solverBounds;
    };
    envs = lib.mapAttrs (_: env: { ghc = managedEnvGhc env; targets = util.env.targets env; }) util.managed.env.envs;
    inherit (config) buildOutputsPrefix;
  };

  cliJson = util.jsonFile "managed-config" cliConfig;

in {
  inherit current cliJson;
}
