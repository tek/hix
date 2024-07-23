{util}: let

  inherit (util) config;

  envApps = env: util.managed.output.appsForEnvs { latest = env.name; lower = env.name; };

in {

  apps = { env = util.mapValues envApps config.envs; };

}
