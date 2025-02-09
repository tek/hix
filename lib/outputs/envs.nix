{util}: let

  inherit (util) config build internal lib;

  systemAllowed = env:
  env.systems == null || (lib.elem config.system env.systems);

  validEnvs = lib.filterAttrs (_: systemAllowed) config.envs;

  validBuildEnvs = internal.envs.mapMaybe (env: a: if systemAllowed env then a else null) build.envs;

  depVersions = env: import ../dep-versions.nix { inherit config lib util env; };

  legacyEnv = env: outputs:
  lib.optionalAttrs util.expose.internals
  { inherit (env.ghc) pkgs ghc; ghc0 = env.ghc.vanillaGhc; }
  ;

  appsEnv = env: outputs: { dep-versions = depVersions env.name; };

in {

  legacyPackages =
    { env = internal.envs.mapMaybe legacyEnv validBuildEnvs; }
    //
    (internal.envs.mapMaybe appsEnv (internal.envs.filterExposed "scoped" validBuildEnvs))
    ;

  shells = util.mapValues (e: e.shell) (internal.envs.filterExposed "shell" validEnvs);

}
