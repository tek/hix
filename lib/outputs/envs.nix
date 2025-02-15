{util}: let

  inherit (util) config build internal lib justIf;

  systemAllowed = env:
  env.systems == null || (lib.elem config.system env.systems);

  validEnvs = lib.filterAttrs (_: env: systemAllowed env && env.enable) config.envs;

  validBuildEnvs = internal.envs.mapMaybe (env: a: justIf (systemAllowed env) a) build.envs;

  depVersions = env: import ../dep-versions.nix { inherit config lib util env; };

  legacyEnv = env: outputs:
  justIf util.expose.internals { inherit (env.ghc) pkgs ghc; ghc0 = env.ghc.vanillaGhc; }
  ;

  appsEnv = env: outputs: { dep-versions = depVersions env.name; };

in {

  legacyPackages =
    { env = internal.envs.mapMaybe legacyEnv validBuildEnvs; }
    //
    (internal.envs.map appsEnv (internal.envs.filterExposed "scoped" validBuildEnvs))
    ;

  shells = util.mapValues (e: e.shell) (internal.envs.filterExposed "shell" validEnvs);

}
