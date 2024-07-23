{util}: let

  inherit (util) build internal;

  mainAppimage = outputs:
  internal.package.setWithExe (exe: { inherit (outputs.executables.${exe.name}) appimage; });

  appimages = let

    envMain = env: pkgs: internal.env.setWithMain (main: pkgs.${main.name}) env // pkgs;

    target = env: pkg: outputs:
    util.maybe {} (mainAppimage outputs) pkg
    //
    util.mapValues (exe: { inherit (exe) appimage; }) outputs.executables
    ;

  in build.bimapTargetsFor "scoped" envMain target;

  legacyDev = env: pkg: outputs: {
    inherit (outputs) cross static release musl executables;
  };

  fullPackage = env: pkg: outputs: { inherit (outputs) cross static musl release executables; } // outputs.package;

  envKeyed =
    util.mergeValues (
    internal.envs.filterExposed "envKeyed" (
      build.targetsPackagesFor "packages" (
        env: _: outputs: { ${env.name} = outputs.package; })));

  envApps = env: pkg: outputs:
  util.mapValues (exe: exe.app) outputs.executables;

  mainPackageOutputs = pkg: let
    name = pkg.name;
    basic = outputs: {
      default = outputs.package;
      inherit (outputs) musl static;
    };
  in
  util.maybe {} basic (build.targets.dev.${name} or null)
  //
  envKeyed.${name} or {}
  ;

  envExes = pkgsExes: pkg: let
    pkgExes = pkgsExes.${pkg.name};
  in pkgExes // internal.package.setWithExe (exe: pkgExes.${exe.name}) pkg;

  appsEnv = env: pkgExes:
  util.mergeAll [
    (util.catValues pkgExes)
    (internal.env.setWithMain (envExes pkgExes) env)
  ];

  apps = build.bimapTargetsFor "apps" appsEnv envApps;

in {

  # This produces scoped sets like:
  # {
  #   hix = {
  #     release = <drv>;
  #     min = <drv>;
  #     ...
  #   };
  #   integration = {
  #     ...
  #   };
  # }
  legacyPackages =
    util.mergeAll [
      (build.targetsFor "packages" legacyDev).dev
      { env = build.mapTargets fullPackage; }
      envKeyed
      { env = appimages; }
      appimages
      appimages.dev
      (build.targetsFor "packages" fullPackage)
    ];

  # Unconditionally exposing the main package, seems reasonable.
  packages =
    (build.targetsFor "packages" (_: _: outputs: outputs.package)).dev
    //
    internal.packages.setWithMain mainPackageOutputs
    ;

  inherit apps;

  checks = internal.env.prefixed (build.targetsFor "checks" (_: _: outputs: outputs.package));

}
