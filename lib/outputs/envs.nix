{util}: let

  inherit (util) config build lib internal;
  inherit (util.internal.env) mapValidatedOutputs;

  depVersions = env: import ../dep-versions.nix { inherit config lib util env; };

  legacyEnv = env:
  lib.optionalAttrs util.expose.internals {
    inherit (env.toolchain) pkgs;
    ghc = env.toolchain.packages;
    ghc0 = env.toolchain.vanilla;
  };

  appsEnv = env: { dep-versions = depVersions env.name; inherit (env) shell; };

  managedEnv = env: {
    solver = internal.ghc.packageDb.solver (!config.managed.internal.localsInPackageDb) env;
  };

  prefixedEnv = env:
  legacyEnv env // appsEnv env;

  prefixed.env = mapValidatedOutputs null prefixedEnv build.envs;

  scoped = mapValidatedOutputs "scoped" appsEnv build.envs;

in {

  legacyPackages = prefixed // scoped;

  shells = mapValidatedOutputs "shell" (e: e.shell) build.envs;

  internal.env = util.mapValues managedEnv util.managed.env.envs;

}
