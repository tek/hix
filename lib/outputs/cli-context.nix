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
    inherit (conf) name env;
    inherit (comp) language;
    extensions = comp.default-extensions;
    ghcOptions = comp.ghc-options;
    prelude = if conf.prelude.enable then conf.prelude else null;
    sourceDirs = comp.source-dirs;
    deps = comp.dependencies;
  };

  packageConf = name: pkg: {
    inherit name;
    src = util.project.packages.${name}.path;
    components = util.mapValues componentConf pkg;
  };

  packages = lib.mapAttrs packageConf util.hpack.conf.normalized.components;

  commandEnv = env: {
    inherit (env) runner;
    ghciArgs = env.ghci.args;
    ghcidArgs = env.ghcid.args;
  };

  commandEnvs = util.mapValues commandEnv config.envs;

  command = env: {
    mainPackage = config.main;
    inherit packages;
    defaultEnv = env.name;
  };

  commands = util.mapValues command config.envs;

  # TODO add to this set:
  # - restarts
  # - cwd
  ghci = env: {
    command = command env;
    mainPackage = config.main;
    inherit packages;
    setup = config.ghci.setup;
    run = config.ghci.run;
    args = config.ghci.args;
    inherit (config) manualCabal;
  };

  ghcis = util.mapValues ghci config.envs;

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

    preproc = {
      packages = if config.manualCabal then null else packages;
    };

  };

in data // {

  json = lib.mapAttrs json data // {
    command = lib.mapAttrs (name: json "command-${name}") commands;

    ghci = lib.mapAttrs (name: json "ghci-${name}") ghcis;

    command-env = lib.mapAttrs (name: json "command-env-${name}") commandEnvs;
  };

  inherit commands commandEnvs ghcis;

}
