{util}: let
  inherit (util) config lib;

  current = let
    file = "${config.base}/${config.managed.file}";
  in
  { bounds = {}; overrides = {}; initial = {}; resolving = false; } //
  lib.optionalAttrs (config.managed.enable && lib.pathExists file) (import file);

  managedEnvGhc =
    util.ghc.packageDbSolver (!config.managed.internal.localsInPackageDb);

  cliConfig = let
    package = name: comps: let
      meta = util.hpack.conf.meta.${name};
    in {
      inherit name;
      inherit (meta) version;
      deps = map util.version.normalize (lib.concatMap (c: c.dependencies) (lib.concatLists (lib.mapAttrsToList (name: comp: if name == "library" then [comp] else lib.attrValues comp) comps)));
    };
  in {
    packages = lib.mapAttrs package util.hpack.conf.components;
    state = util.managed.state.current;
    envs = lib.mapAttrs (_: env: { ghc = managedEnvGhc env; targets = util.env.targets env; }) util.managed.env.envs;
    inherit (config) buildOutputsPrefix;
  };

  cliJson = util.jsonFile "managed-config" cliConfig;

in {
  inherit current cliJson;
}
