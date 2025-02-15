{util}: let

  inherit (util) config lib internal;

  cabalConfig = config.hpack.internal.packages;

  nix-appimage = import (builtins.fetchTarball {
    url = "https://github.com/ralismark/nix-appimage/archive/46aef0153eb833e0b896b60ca2bdb6354b410e45.tar.gz";
    sha256 = "14v9hzfj0x12xsqxw77qivyknc39zmck5rsji88virbpmyrnsh0g";
  });

  release = import ../release-derivation.nix { inherit util; };

  cross = ghc: name: let
    arch = _: pkgs: let
      drv = pkgs.hixPackages.${name};
    in drv // { static = util.hsLib.justStaticExecutables drv; };
  in
  lib.mapAttrs arch ghc.pkgs.pkgsCross;

  appimageDerivation = nix-appimage.bundlers.${config.system}.default;

  exeDrv = name: drv:
  drv.overrideAttrs { pname = name; meta.mainProgram = name; };

  staticExeDrv = name: drv:
  exeDrv name (util.hsLib.justStaticExecutables drv);

  executables = drv: muslDrv: staticDrv: cabal: pkg: let

    executable = name: _: let
      package = exeDrv name drv;
      musl = staticExeDrv name muslDrv;
      static = staticExeDrv name staticDrv;
    in {
      app = util.app "${package}/bin/${name}";
      inherit package static musl;
      appimage = appimageDerivation musl;
    };

    mainExe = main: let
      exe = executable main.name null;
    in { ${pkg.name} = exe; };

  in
  internal.package.setWithExe mainExe pkg
  //
  lib.mapAttrs executable cabal.executables or {};

  nonlocal = {
    cabal = {};
    expose = {};
    executables = {};
  };

  local = env: generic: pkg: let
    cabal = cabalConfig.${pkg.name};
  in {
    inherit cabal;
    inherit (pkg) expose;
    executables = executables generic.package generic.musl generic.static cabal pkg;
  };

  buildPackage = env: pkgName: let
    ghc = env.ghc.ghc;
    package = ghc.${pkgName};
    generic = {
      inherit package ghc;
      static = env.ghc.pkgs.pkgsStatic.hixPackages.${pkgName};
      musl = env.ghc.pkgs.pkgsMusl.hixPackages.${pkgName};
      cross = cross env.ghc pkgName;
      release = release package;
    };
  in
  generic //
  util.maybeNull nonlocal (local env generic) (config.packages.${pkgName} or null)
  ;

  buildPackages = envName: env: let
    all = config.internal.packageNames ++ config.output.extraPackages;
    existing = lib.filter (p: env.ghc.ghc ? ${p}) all;
  in lib.genAttrs existing (buildPackage env);

in lib.mapAttrs buildPackages config.envs
