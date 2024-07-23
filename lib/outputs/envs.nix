{util}: let

  inherit (util) config build internal lib;

  systemAllowed = env:
  env.systems == null || (lib.elem config.system env.systems);

  validEnvs = lib.filterAttrs (_: systemAllowed) config.envs;

  validBuildEnvs = internal.envs.mapMaybe (env: a: if systemAllowed env then a else null) build.envs;

  depVersions = env: import ../dep-versions.nix { inherit config lib util env; };

  legacyEnv = env: outputs:
  { inherit (env.ghc) pkgs ghc; ghc0 = env.ghc.vanillaGhc; }
  //
  outputs
  ;

  appsEnv = env: outputs: { dep-versions = util.app (depVersions env.name); };

in {

  legacyPackages.env = internal.envs.mapMaybe legacyEnv validBuildEnvs;

  apps = internal.envs.mapMaybe appsEnv (internal.envs.filterExposed "apps" validBuildEnvs);

  shells = util.mapValues (e: e.shell) (internal.envs.filterExposed "shell" validEnvs);

}
