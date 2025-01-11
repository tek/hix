{util}: let

  inherit (util) config;

  envApps = env: util.managed.output.appsForEnvs { latest = env.name; lower = env.name; };

in {

  legacyPackages =
    util.mergeAll [
      { env = util.mapValues envApps config.envs; }
      util.managed.output.apps
      util.managed.output.gen
    ];

}
