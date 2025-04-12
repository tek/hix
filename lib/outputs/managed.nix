{util}: let

  inherit (util) config;

  envApps = env: util.managed.output.scopedAppsForEnvs { latest = env.name; lower = env.name; };

in {

  legacyPackages =
    util.mergeAll [
      { env = util.mapValues envApps config.envs; }
      util.managed.output.legacyApps
      util.managed.output.gen
    ];

  apps = util.managed.output.apps;

}
