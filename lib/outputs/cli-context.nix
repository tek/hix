{util}: let

  inherit (util) config lib internal project;

  packageDeps = comps: let
    handleMainLib = name: comp:
    if name == "library"
    then [comp]
    else lib.attrValues comp;
  in
    lib.concatMap (c: c.dependencies) (lib.concatLists (lib.mapAttrsToList handleMainLib comps));

  managedPackage = name: comps: let
    meta = util.hpack.conf.meta.${name};
  in {
    inherit name;
    inherit (meta) version;
    deps = map util.version.normalize (packageDeps comps);
  };

  managedEnv = env: {
    targets = internal.env.targets env;
    ghc = null;
  };

  maintEnv = internal.env.targets;

  maintPackage = name: conf: let
    comps = util.hpack.conf.components.${name};
    meta = util.hpack.conf.meta.${name};
  in {
    package = {
      inherit name;
      inherit (meta) version;
      deps = map util.version.normalize (packageDeps comps);
    };
    path = project.packages.${name}.path;
  };

  data = {

  managed = {
    packages = lib.mapAttrs managedPackage util.hpack.conf.components;
    state = util.managed.state.current;
    envs = util.mapValues managedEnv util.managed.env.envs;
    hackage = config.hackage.repos;
  };

  maint = {
    packages = lib.mapAttrs maintPackage config.packages;
    hackage = config.hackage.repos;
    envs = util.mapValues maintEnv util.managed.env.envs;
  };

  };

in data // {
  json = lib.mapAttrs (k: v: util.jsonFile "context-${k}" v) data;
}
