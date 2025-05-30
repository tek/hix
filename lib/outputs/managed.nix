{util}: let

  inherit (util) config internal;
  inherit (internal) managed;

  envApps = env: managed.output.scopedAppsForEnvs { latest = env.name; lower = env.name; };

in {

  legacyPackages =
    util.mergeAll [
      { env = util.mapValues envApps config.envs; }
      managed.output.legacyApps
      managed.output.gen
    ];

  apps = managed.output.apps;

}
