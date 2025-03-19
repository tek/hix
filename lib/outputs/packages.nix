{util}: let

  inherit (util) build internal lib;

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

  muslOutput = outputs:
  lib.optionalAttrs (util.config.system == "x86_64-linux") {
    inherit (outputs) musl;
  };

  staticOutputs = outputs:
  lib.optionalAttrs util.expose.static (
    { inherit (outputs) static; }
    //
    muslOutput outputs
  );

  specialOutputs = outputs:
  lib.optionalAttrs util.expose.cross { inherit (outputs) cross; }
  //
  staticOutputs outputs
  //
  { inherit (outputs) release; }
  ;

  legacyDev = env: _pkg: outputs:
  specialOutputs outputs
  //
  { inherit (outputs) executables; }
  ;

  fullPackage = env: pkg: outputs: legacyDev env pkg outputs // outputs.package;

  # Expose packages using the path `<package>.<env>`.
  # Expose keys `packages` (for packages) and `envKeyed` (for envs, defaults to `false`).
  # Uses vanilla derivations.
  # The purpose is to expose derivations from special envs, like `min` and `profiled`:
  # `{ legacyPackages.hix = { min = <drv>; profiled = <drv>; } }`
  # These derivations used to be attached to the `build.packages` nodes, like `cross` and `musl` are, but since the
  # effects in `min` and `profiled` are caused by overrides, they are env-specific, so they can't just be added to all
  # package outputs.
  envKeyed =
    util.mergeValues (
      internal.envs.filterExposed "envKeyed" (
        build.targetsPackagesFor "packages" (
          env: _: outputs: { ${env.name} = outputs.package; }
        )
      )
    );

  envApps = env: _pkg: outputs:
  util.mapValues (exe: exe.app) outputs.executables;

  mainPackageOutputs = pkg: let
    name = pkg.name;
    basic = outputs:
    { default = outputs.package; }
    //
    staticOutputs outputs;
  in
  util.maybe {} basic (util.justAttr name build.targets.dev)
  //
  envKeyed.${name} or {}
  ;

  envExes = pkgsExes: main: let
    mainExes = pkgsExes.${main.name};
  in mainExes // internal.package.setWithExe (exe: mainExes.${exe.name}) main;

  appsEnv = env: pkgExes:
  util.mergeAll [
    (util.catValues pkgExes)
    (internal.env.setWithMain (envExes pkgExes) env)
  ];

  envExecutables = env: packages: let
    packageExecutables = util.mapValues (outputs: outputs.executables) packages;
    exePackages = util.mapValues (exe: exe.package);
    executables = lib.concatMapAttrs (_: exePackages) packageExecutables;
    mainOutputs = name: specialOutputs packages.${name} // exePackages packageExecutables.${name};
  in
  { inherit executables; }
  //
  executables
  //
  internal.env.setWithMain (main: mainOutputs main.name) env
  //
  packages
  ;

  unrestricted.env = build.bimapTargets envExecutables fullPackage;

  scoped = build.bimapTargetsFor "scoped" envExecutables fullPackage;

  # This excludes the main derivation at the root of each package because those are already present in
  # `outputs.packages`.
  # It uses the exposure key `packages` because the outputs aren't contained in env-key scopes.
  devOnly = (build.targetsFor "packages" legacyDev).dev;

  appimage = [
    { env = appimages; }
    appimages
    appimages.dev
  ];

  legacyPackages = [
    envKeyed
    unrestricted
    scoped
    devOnly
  ]
  ++
  lib.optionals util.expose.appimage appimage
  ;

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
  legacyPackages = util.mergeAll legacyPackages;

  # Unconditionally exposing the main package, seems reasonable.
  packages =
    (build.targetsFor "packages" (_: _: outputs: outputs.package)).dev
    //
    internal.packages.setWithMain mainPackageOutputs
    ;

  apps = build.bimapTargetsFor "apps" appsEnv envApps;

  checks = internal.env.prefixed (build.targetsFor "checks" (_: _: outputs: outputs.package));

}
