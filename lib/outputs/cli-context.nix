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

  componentConf = comp: let
    conf = comp.__conf;
  in {
    inherit (conf) name;
    inherit (comp) language;
    extensions = comp.default-extensions;
    ghcOptions = comp.ghc-options;
    prelude = if conf.prelude.enable then conf.prelude else null;
    runner = if conf.env == null then null else config.envs.${conf.env}.runner;
    sourceDirs = comp.source-dirs;
    deps = comp.dependencies;
  };

  packageConf = name: pkg: {
    inherit name;
    src = util.project.packages.${name}.path;
    components = util.mapValues componentConf pkg;
  };

  packages = lib.mapAttrs packageConf util.hpack.conf.normalized.components;

  envConf = default: {
    mainPackage = config.main;
    inherit packages;
    defaultEnv = default.runner;
  };

  # TODO add to this set:
  # - component-dependent ghci args
  # - restarts
  # - cwd
  ghci = env: {
    env = envConf env;
    mainPackage = config.main;
    inherit packages;
    setup = config.ghci.setup;
    run = config.ghci.run;
    args = config.ghci.args;
    inherit (config) manualCabal;
  };

  json = k: v: util.jsonFile "context-${k}" v;

  data = {

    inherit packages;

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

  json = lib.mapAttrs json data // {
    ghci = env: json "ghci" (ghci env);
  };

  inherit ghci;

}
