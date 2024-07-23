{util}: let

  inherit (util) internal build lib;

  exposedPackages = purpose:
  internal.envs.filterExposedPackages purpose build.packages;

  exposed = purpose:
  internal.envs.filterExposed purpose (exposedPackages purpose);

  bimapTargets = fe: fp: internal.envs.bimapTargets fe fp build.packages;

  mapTargets = f: internal.envs.mapTargets f build.packages;

  targets = mapTargets (_: _: a: a);

  targetsFor = purpose: f:
  internal.envs.mapTargets f (exposed purpose);

in {

  inherit exposedPackages exposed bimapTargets mapTargets targets targetsFor;

  targetsPackagesFor = purpose: f:
  internal.envs.mapTargets f (exposedPackages purpose);

  targetsExposedPackages = purpose:
  internal.envs.filterExposedPackages purpose targets;

  targetsExposed = purpose:
  targetsFor purpose lib.id;

  bimapTargetsFor = purpose: fe: fp: internal.envs.bimapTargets fe fp (exposed purpose);

}
