{util}: let
  inherit (util) config lib internal;

  current = let
    file = "${config.base}/${config.managed.file}";
  in
  { bounds = {}; overrides = {}; initial = {}; resolving = false; } //
  lib.optionalAttrs (config.managed.enable && lib.pathExists file) (import file);

  managedEnvGhc =
    util.ghc.packageDbSolver (!config.managed.internal.localsInPackageDb);

  packageDeps = comps: let
    handleMainLib = name: comp:
    if name == "library"
    then [comp]
    else lib.attrValues comp;
  in
    lib.concatMap (c: c.dependencies) (lib.concatLists (lib.mapAttrsToList handleMainLib comps));

  package = name: comps: let
    meta = util.hpack.conf.meta.${name};
  in {
    inherit name;
    inherit (meta) version;
    deps = map util.version.normalize (packageDeps comps);
  };

  envConfig = env: {
    ghc = managedEnvGhc env;
    targets = internal.env.targets env;
  };

  cliConfig = {
    packages = lib.mapAttrs package util.hpack.conf.components;
    state = util.managed.state.current;
    envs = util.mapValues envConfig util.managed.env.envs;
  };

  cliJson = util.jsonFile "managed-config" cliConfig;

in {
  inherit current cliJson;
}
