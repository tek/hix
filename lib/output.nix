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

  envPackageExeFor = cons: env: pkgName: let
    pkg = config.packages.${pkgName};
    exe = pkgMainExe pkg;
  in lib.optionalAttrs (exe != null) { ${pkgName} = cons env pkg exe.name; };

  envPackageExes = cons: env:
    foldExes (cons env) //
    util.attrsetMain (envPackageExeFor cons env);

  envExes = env:
  {
    executables.static = envPackageExes staticPackageExe env;
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
  }) // envExes env;

  prefixedInEnv = v: lib.mapAttrs' (n: d: { name = "${v}-${n}"; value = d; }) (envOutputs v);

  prefixedInEnvs = envs: util.foldMapAttrs prefixedInEnv envs;

  prefixedEnvDerivations = env:
  lib.mapAttrs' (n: d: { name = "${env}-${n}"; value = d; }) config.envs.${env}.derivations;

  devOutputs = let
    env = config.envs.dev;
    ghc = env.ghc.ghc;
    minGhc = config.envs.min.ghc.ghc;
    profiledGhc = config.envs.profiled.ghc.ghc;
    extra = name: pkg: withCross env name // {
      release = releaseDrv ghc.${name};
      min = minGhc.${name};
      profiled = profiledGhc.${name};
    };
    local = lib.mapAttrs extra config.envs.dev.derivations;
  in local // util.attrsetMain (main: {
    default = local.${main};
    min = local.${main}.min;
    profiled = local.${main}.profiled;
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
    lower = {
      init = app (libManaged.lowerInit [lower]);
      optimize = app (libManaged.lowerOptimize [lower]);
    };
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
    lower = {
      init = managedCmdMulti "lower.init" "lower" libManaged.lowerInit sets;
      optimize = managedCmdMulti "lower.optimize" "lower" libManaged.lowerOptimize sets;
    };
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

  managedGaWorkflow = sort: config.pkgs.writeText "${sort}.yaml" ''
    name: ${sort}
    on: workflow_dispatch
    permissions:
      contents: write
      pull-requests: write
    jobs:
      bump-pr:
        runs-on: ubuntu-latest
        steps:
        - uses: actions/checkout@v4
        - uses: DeterminateSystems/nix-installer-action@main
          with:
            extra-conf: |
              access-tokens = github.com=''${{ secrets.GITHUB_TOKEN }}
        - uses: DeterminateSystems/magic-nix-cache-action@main
        - id: bump
          run: nix run .#${sort} -- --output=ga-pr
        - name: pr
          if: steps.bump.outputs.commit-message
          uses: peter-evans/create-pull-request@v5
          with: ''${{ steps.bump.outputs }}
  '';

  genManagedGaWorkflow = sort: let
    script = config.pkgs.writeScript "gen-managed-ga-workflow" ''
    dir=$PWD/.github/workflows
    mkdir -p $dir
    cp ${managedGaWorkflow sort} $dir/${sort}.yaml
    '';
  in app script;

  genManaged = {
    managed = {
      gen.ga = {
        bump = genManagedGaWorkflow "bump";
        lower = genManagedGaWorkflow "lower.optimize";
      };
    };
  };

  # We have lots of nested attrsets in the apps output, and since Nix requires all top-level apps attrs to implement the
  # app interface, we add a dummy app to each attr (and recursively, though not strictly necessary).
  addDummyApps = let
    isAppAttr = n: n == "type" || n == "program";
    add = pre: name: a: let
      newPre = pre ++ [name];
      sub = lib.filter (n: !isAppAttr n) (lib.attrNames a);
    in
    if isAppAttr name
    then a
    else
    if a ? program
    then spin newPre a
    else util.dummyApp newPre sub // spin newPre a;
    spin = pre: lib.mapAttrs (add pre);
  in
    spin [];

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
  genManaged
  addDummyApps
  ;
}
