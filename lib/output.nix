{ lib, config, util, ... }:
let
  inherit (util) app;

  envCommand = import ./command.nix { inherit config util; };

  depVersions = env: import ./dep-versions.nix { inherit config lib util env; };

  releaseDrv = import ./release-derivation.nix {
    inherit lib;
    inherit (util) hsLib;
  };

  libManaged = import ./managed.nix { inherit util; };

  console = import ./console.nix { inherit lib; };

  staticDrv = util.hsLib.justStaticExecutables;

  onlyStatic = pkg: { static = staticDrv pkg; };

  withStatic = pkg: pkg // onlyStatic pkg;

  # TODO this needs to be refactored into the core
  allExes = pkg: lib.optionalAttrs pkg.executable.enable { ${pkg.name} = pkg.executable; } //
  lib.filterAttrs (_: e: e.enable) pkg.executables;

  pkgMainExe = pkg: let
    all = allExes pkg;
    names = lib.attrNames all;
  in
  if lib.length names > 0
  then all.${lib.head names}
  else null;

  mainExe = util.withMainOr null (main: pkgMainExe config.packages.${main});

  ifMainExe = lib.optionalAttrs (mainExe != null);

  foldExes = f: let
    pkgExes = pkg: lib.genAttrs (lib.attrNames (allExes pkg)) (f pkg);
  in util.foldMapAttrs pkgExes (lib.attrValues config.packages);

  staticPackageExe = env: pkg: exe: staticDrv (env.ghc.ghc.${pkg.name}.overrideAttrs (_: { pname = exe; }));

  staticPackageExeFor = env: pkgName: let
    pkg = config.packages.${pkgName};
    exe = pkgMainExe pkg;
  in lib.optionalAttrs (exe != null) { ${pkgName} = staticPackageExe env pkg exe.name; };

  staticExes = env:
  {
    executables.static =
      foldExes (staticPackageExe env) //
      util.attrsetMain (staticPackageExeFor env);
  };

  cross = ghc: name: let
    mkCross = cname: cpkgs: withStatic cpkgs.hixPackages.${name};
  in lib.mapAttrs mkCross ghc.pkgs.pkgsCross;

  withCross = env: name: let
    ghc = env.ghc;
  in withStatic ghc.ghc.${name} // { cross = cross ghc name; };

  envOutputs = v: let
    env = config.envs.${v};
  in lib.mapAttrs (n: d: (withCross env n)) env.derivations;

  fullEnvOutputs = v: let
    env = config.envs.${v};
  in envOutputs v // util.attrsetMain (main: {
    static = staticDrv env.derivations.${main};
  }) // staticExes env;

  prefixedInEnv = v: lib.mapAttrs' (n: d: { name = "${v}-${n}"; value = d; }) (envOutputs v);

  prefixedInEnvs = envs: util.foldMapAttrs prefixedInEnv envs;

  prefixedEnvDerivations = env:
  lib.mapAttrs' (n: d: { name = "${env}-${n}"; value = d; }) config.envs.${env}.derivations;

  devOutputs = let
    env = config.envs.dev;
    ghc = env.ghc.ghc;
    minGhc = config.envs.min.ghc.ghc;
    extra = name: pkg: withCross env name // {
      release = releaseDrv ghc.${name};
      min = minGhc.${name};
    };
    local = lib.mapAttrs extra config.envs.dev.derivations;
  in local // util.attrsetMain (main: {
    default = local.${main};
    min = local.${main}.min;
    static = staticDrv local.${main};
  });

  scopedEnvOutputs = envs: lib.genAttrs envs envOutputs;

  envsApi = envs: { env = lib.mapAttrs (n: e: { inherit (e.ghc) pkgs ghc; } // fullEnvOutputs n) envs; };

  appimage = env: name: config.pkgs.writeScript "hix-appimage-${name}" ''
  set -eu
  outlink="hix-appimage-tmp-${name}"
  ${config.pkgs.nix}/bin/nix \
    --extra-experimental-features 'nix-command flakes' \
    bundle -o $outlink --bundler github:ralismark/nix-appimage '${config.base}#env.${env}.executables.static.${name}'
  store_path=$(${config.pkgs.coreutils}/bin/readlink $outlink)
  rm $outlink
  echo -e "${console.chevrons} AppImage bundle for ${console.bold (console.color console.colors.blue name)} created at:" >&2
  echo $store_path
  '';

  appimageApp = env: name: { appimage = app (appimage env name); };

  envAppimages = env: let
    mainAppimage = appimageApp env.name mainExe.name;
  in
  util.attrsetMain (main: ifMainExe (mainAppimage // { ${main} = mainAppimage; })) //
  foldExes (pkg: n: appimageApp env.name n);

  mainAppimageApp = ifMainExe (appimageApp "dev" mainExe.name);

  envCommands = env:
  lib.mapAttrs (_: command: app ((envCommand { inherit env command; }).path)) config.commands;

  managedAll = latest: lower: {
    bump = app (libManaged.bump [latest]);
    lower.init = app (libManaged.lowerInit [lower]);
    lower.optimize = app (libManaged.lowerOptimize [lower]);
  };

  envApps = env: {
    ${env.name} =
      envCommands env //
      envAppimages env //
      managedAll env.name env.name //
      { dep-versions = app (depVersions env.name); };
  };

  managedEnvGhcs = lib.mapAttrs (_: env: { ghc-local = util.ghc.packageDbLocal env; }) util.managed.envs;

  commandApps = lib.mapAttrs (_: c: app c.path);

  wantManagedChecks = config.managed.enable && config.managed.check;

  managedChecks =
    lib.optionalAttrs wantManagedChecks
    (util.foldMapAttrs prefixedEnvDerivations (lib.attrNames util.managed.envs))
    ;

  managedCmdMulti = prefix: envSort: mk: sets: let
    envName = name: "${envSort}-${name}";
    envs = map (name: envName name) sets;
  in
  lib.genAttrs sets (name: app (mk [(envName name)])) //
  app (mk envs);

  managedMulti = sets: {
    bump = managedCmdMulti "bump" "latest" libManaged.bump sets;
    lower.init = managedCmdMulti "lower.init" "lower" libManaged.lowerInit sets;
    lower.optimize = managedCmdMulti "lower.optimize" "lower" libManaged.lowerOptimize sets;
  };

  # We're not guarding this with a `optionalAttrs` so that we can print an error when the user executes an app.
  # TODO Add an override option that opts into removing these apps from the flake.
  managedApps =
    if config.managed.sets == "all"
    then managedAll "latest" "lower"
    else if config.managed.sets == "each"
    then managedMulti config.internal.packageNames
    else managedMulti (lib.attrNames config.managed.sets)
    ;

in {
  inherit
  prefixedInEnvs
  scopedEnvOutputs
  devOutputs
  envsApi
  app
  appimageApp
  mainAppimageApp
  envApps
  commandApps
  mainExe
  pkgMainExe
  managedChecks
  managedApps
  managedEnvGhcs
  ;
}
