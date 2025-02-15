{util}: let

  inherit (util) internal build lib;

  packages = internal.envs.filterEnabled build.packages;

  # exposedPackages ::
  #   Map EnvName (Map PackageName BuildPackage)
  exposedPackages = purpose:
  internal.envs.filterExposedPackages purpose packages;

  # Exposed envs with exposed packages.
  # exposed ::
  #   Map EnvName (Map PackageName BuildPackage)
  exposed = purpose:
  internal.envs.filterExposed purpose (exposedPackages purpose);

  # bimapTargets ::
  #   (Maybe Env -> Map PackageName b -> c) ->
  #   (Maybe Env -> Package -> BuildPackage -> b) ->
  #   Map EnvName c
  bimapTargets = fe: fp:
  internal.envs.bimapTargets fe fp packages;

  # bimapMaybeTargets ::
  #   (Maybe Env -> Map PackageName b -> Maybe c) ->
  #   (Maybe Env -> Package -> BuildPackage -> Maybe b) ->
  #   Map EnvName c
  bimapMaybeTargets = fe: fp:
  internal.envs.bimapMaybeTargets fe fp packages;

  mapTargets = f: internal.envs.mapTargets f packages;

  mapMaybeTargets = f: internal.envs.mapMaybeTargets f packages;

  targets = mapTargets (_: _: a: a);

  # targetsFor ::
  #   String ->
  #   (Maybe Env -> Package -> BuildPackage -> b) ->
  #   Map EnvName (Map PackageName b)
  targetsFor = purpose: f:
  internal.envs.mapTargets f (exposed purpose);

  # targetsMaybeFor ::
  #   String ->
  #   (Maybe Env -> Package -> BuildPackage -> Maybe b) ->
  #   Map EnvName (Map PackageName b)
  targetsMaybeFor = purpose: f:
  internal.envs.mapMaybeTargets f (exposed purpose);

in {

  inherit bimapTargets bimapMaybeTargets mapTargets mapMaybeTargets targets targetsFor targetsMaybeFor;

  targetsPackagesFor = purpose: f:
  internal.envs.mapTargets f (exposedPackages purpose);

  targetsExposed = purpose:
  targetsFor purpose lib.id;

  bimapTargetsFor = purpose:
  fe: fp: internal.envs.bimapTargets fe fp (exposed purpose);

  bimapMaybeTargetsFor = purpose:
  fe: fp: internal.envs.bimapMaybeTargets fe fp (exposed purpose);

}
