{ lib, config, util, ... }:
let
  inherit (util) app;

  envCommand = import ../lib/command.nix { inherit config util; };

  depVersions = env: import ../lib/dep-versions.nix { inherit config lib util env; };

  releaseDrv = import ../lib/release-derivation.nix {
    inherit lib;
    inherit (util) hsLib;
  };

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
  if lib.hasAttr pkg.name all
  then all.${pkg.name}.name
  else if lib.length names > 0
  then all.${lib.head names}
  else pkg.executable.name;

  mainExe = pkgMainExe config.packages.${config.main};

  foldExes = f: let
    pkgExes = pkg: lib.genAttrs (lib.attrNames (allExes pkg)) (f pkg);
  in util.foldMapAttrs pkgExes (lib.attrValues config.packages);

  staticPackageExe = env: pkg: exe: staticDrv (env.ghc.ghc.${pkg.name}.overrideAttrs (_: { pname = exe; }));

  staticExes = env:
  {
    executables.static =
      foldExes (staticPackageExe env) //
      { ${config.main} = staticPackageExe env config.packages.${config.main} mainExe.name; };
  };

  cross = ghc: name: let
    mkCross = cname: cpkgs: withStatic cpkgs.hixPackages.${name};
  in lib.mapAttrs mkCross ghc.pkgs.pkgsCross;

  withCross = env: name: let
    ghc = env.ghc;
  in withStatic ghc.ghc.${name} // { cross = cross ghc name; };

  envDerivations = v: let
    env = config.envs.${v};
  in lib.mapAttrs (n: d: (withCross env n)) env.derivations;

  fullEnvDerivations = v: let
    env = config.envs.${v};
  in envDerivations v // {
    static = staticDrv env.derivations.${config.main};
  } // staticExes env;

  prefixedEnvDerivations = envs: let
    envPkgs = v: lib.mapAttrs' (n: d: { name = "${v}-${n}"; value = d; }) (envDerivations v);
  in util.foldMapAttrs envPkgs envs;

  devOutputs = let
    env = config.envs.dev;
    ghc = env.ghc.ghc;
    minGhc = config.envs.min.ghc.ghc;
    extra = name: pkg: withCross env name // {
      release = releaseDrv ghc.${name};
      min = minGhc.${name};
    };
    local = lib.mapAttrs extra config.envs.dev.derivations;
  in local // {
    default = local.${config.main};
    min = local.${config.main}.min;
    static = staticDrv local.${config.main};
  };

  scopedEnvDerivations = envs: lib.genAttrs envs envDerivations;

  envsApi = envs: { env = lib.mapAttrs (n: e: { inherit (e.ghc) pkgs ghc; } // fullEnvDerivations n) envs; };

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

  envAppimages = env:
  appimageApp env.name mainExe.name //
  { ${config.main} = appimageApp env.name config.main; } //
  foldExes (pkg: n: appimageApp env.name n);

  mainAppimageApp = appimageApp "dev" mainExe.name;

  envCommands = env:
  lib.mapAttrs (_: command: app "${(envCommand { inherit env command; }).path}") config.commands;

  envApps = env: {
    ${env.name} = envCommands env // envAppimages env // {
      dep-versions = app "${depVersions env.name}";
    };
  };

  commandApps = lib.mapAttrs (_: c: app "${c.path}");

in {
  inherit
  prefixedEnvDerivations
  scopedEnvDerivations
  devOutputs
  envsApi
  app
  appimageApp
  mainAppimageApp
  envApps
  commandApps
  mainExe
  pkgMainExe
  ;
}
