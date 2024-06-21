{util}:
let
  inherit (util) app lib config;

  envCommand = import ./command.nix { inherit config util; };

  depVersions = env: import ./dep-versions.nix { inherit config lib util env; };

  releaseDrv = import ./release-derivation.nix {
    inherit lib;
    inherit (util) hsLib;
  };

  console = import ./console.nix { inherit lib; };

  staticDrv = util.hsLib.justStaticExecutables;

  onlyStatic = pkg: { static = staticDrv pkg; };

  withStatic = pkg: pkg // onlyStatic pkg;

  # TODO this needs to be refactored into the core
  allExes = pkg: lib.optionalAttrs pkg.executable.enable { ${pkg.executable.name} = pkg.executable; } //
  lib.filterAttrs (_: e: e.enable) pkg.executables;

  pkgMainExe = pkg: let
    all = allExes pkg;
    names = lib.attrNames all;
  in
  if pkg.executable.enable
  then pkg.executable
  else if lib.length names > 0
  then all.${lib.head names}
  else null;

  mainExe = util.withMainOr null (main: pkgMainExe config.packages.${main});

  ifMainExe = lib.optionalAttrs (mainExe != null);

  foldExes = f: let
    pkgExes = pkg: lib.genAttrs (lib.attrNames (allExes pkg)) (f pkg);
  in util.mapListCatAttrs pkgExes (lib.attrValues config.packages);

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

  nativeMusl = ghc: name:
  withStatic ghc.pkgs.pkgsMusl.hixPackages.${name};

  withCross = env: name: let
    ghc = env.ghc;
  in withStatic ghc.ghc.${name} // { cross = cross ghc name; musl = nativeMusl ghc name; };

  envOutputs = purpose: envName: let
    env = config.envs.${envName};
    extra = n: d:
    if lib.elem purpose ["checks" "compat"]
    then d
    else withCross env n;
  in lib.mapAttrs extra (util.env.derivations purpose envName);

  fullEnvOutputs = purpose: envName: let
    env = config.envs.${envName};
    drvs = util.env.derivations purpose envName;
  in
  envOutputs purpose envName //
  util.attrsetMain (main: { static = staticDrv drvs.${main}; }) //
  envExes env
  ;

  prefixed = purpose: envName:
  lib.mapAttrs' (n: d: { name = "${envName}-${n}"; value = d; }) (envOutputs purpose envName);

  prefixedInEnvs = purpose: envs: util.mapListCatAttrs (prefixed purpose) envs;

  scopedEnvOutputs = envs: lib.genAttrs envs (envOutputs "scoped");

  # TODO the main package/exe should probably also respect the targets and expose settings
  devPackages = let
    env = config.envs.dev;
    ghc = env.ghc.ghc;
    minGhc = config.envs.min.ghc.ghc;
    profiledGhc = config.envs.profiled.ghc.ghc;
    extra = name: pkg: withCross env name // {
      release = releaseDrv ghc.${name};
      min = minGhc.${name};
      profiled = profiledGhc.${name};
    };
    local = lib.mapAttrs extra (util.env.derivations "packages" "dev");
  in local // util.attrsetMain (main: {
    default = local.${main};
    min = local.${main}.min;
    profiled = local.${main}.profiled;
    static = staticDrv local.${main};
  });

  envsApi = envs: {
    env = lib.mapAttrs (n: e: { inherit (e.ghc) pkgs ghc; ghc0 = e.ghc.vanillaGhc; } // fullEnvOutputs "scoped" n) envs;
  };

  appimage = env: name: util.script "hix-appimage-${name}" ''
  set -u
  outlink="hix-appimage-tmp-${name}"
  ${config.pkgs.nix}/bin/nix \
    --extra-experimental-features 'nix-command flakes' \
    bundle -o $outlink --bundler github:ralismark/nix-appimage/8811055025f836f67092a7831993c0b25d485661 '${config.base}#env.${env}.executables.static.${name}'
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

  envApps = env: {
    ${env.name} =
      envCommands env //
      envAppimages env //
      util.managed.output.appsForEnvs { latest = env.name; lower = env.name; } //
      { dep-versions = app (depVersions env.name); };
  };

  commandApps = lib.mapAttrs (_: c: app c.path);

  # We have lots of nested attrsets in the apps output, and since Nix requires all top-level apps attrs to implement the
  # app interface, we add a dummy app to each attr (and recursively, though not strictly necessary) that prints an
  # informational message.
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
  envOutputs
  scopedEnvOutputs
  prefixedInEnvs
  devPackages
  envsApi
  app
  appimageApp
  mainAppimageApp
  envApps
  commandApps
  mainExe
  pkgMainExe
  addDummyApps
  ;
}
