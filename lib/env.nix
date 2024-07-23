{util}: let

  inherit (util) config lib;

  scopedApps = apps:
  lib.optionalAttrs config.output.envApps
  { env = lib.filterAttrs (envName: _: !config.envs.${envName}.hideApps) apps; };

  genScopedApps = f:
  lib.optionalAttrs config.output.envApps
  { env = util.mapValues f (lib.filterAttrs (_: env: !env.hideApps) config.envs); };

in {
  inherit
  scopedApps
  genScopedApps
  ;
}
