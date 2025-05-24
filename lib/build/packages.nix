{util}: let

  inherit (util) config lib internal build maybe justAttr;

  cabalConfig = config.hpack.internal.packages;

  nix-appimage = import (builtins.fetchTarball {
    url = "https://github.com/ralismark/nix-appimage/archive/46aef0153eb833e0b896b60ca2bdb6354b410e45.tar.gz";
    sha256 = "14v9hzfj0x12xsqxw77qivyknc39zmck5rsji88virbpmyrnsh0g";
  });

  release = import ../release-derivation.nix { inherit util; };

  packageSet = pkgs: pkgs.__hix.packages;

  cross = toolchain: name: let
    arch = _: pkgs: let
      drv = (packageSet pkgs).${name} or null;
    in lib.optionalAttrs (drv != null) (drv // { static = util.hsLib.justStaticExecutables drv; });
  in
  lib.mapAttrs arch toolchain.pkgs.pkgsCross;

  appimageDerivation = nix-appimage.bundlers.${config.system}.default;

  binAttr = name: drv: { exe = "${drv}/bin/${name}"; };

  exeDrv = name: drv:
  drv.overrideAttrs (old: {
    pname = name;
    meta.mainProgram = name;
    passthru = old.passthru or {} // binAttr name drv;
  });

  staticExeDrv = name: drv:
  exeDrv name (util.hsLib.justStaticExecutables drv);

  executables = drv: muslDrv: staticDrv: cabal: pkg: let

    executable = name: _: let
      package = exeDrv name drv;
      musl = staticExeDrv name muslDrv;
      static = staticExeDrv name staticDrv;
    in binAttr name drv // {
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

  local = outputs: pkg: let
    cabal = cabalConfig.${pkg.name};
  in {
    inherit cabal;
    inherit (pkg) expose;
    executables = lib.optionalAttrs outputs.exists (executables outputs.package outputs.musl outputs.static cabal pkg);
  };

  buildPackage = env: pkgName: let

    tc = env.toolchain;

    ghc = tc.packages;

    package = ghc.${pkgName} or null;

    exists = package != null;

    general = {
      inherit exists ghc package;
      static = (packageSet tc.pkgs.pkgsStatic).${pkgName} or null;
      musl = (packageSet tc.pkgs.pkgsMusl).${pkgName} or null;
      cross = cross tc pkgName;
      release = lib.mapNullable release package;
    };

    special = maybe nonlocal (local general) (justAttr pkgName config.packages);

  in general // special;

  buildPackages = envName: env: let

    envPackages = util.fromMaybeNull [] env.packages;

    all = internal.project.packageNames ++ config.output.extraPackages ++ envPackages;

    nonNull = lib.filter (p: p != null) all;

  in lib.genAttrs nonNull (buildPackage env);

in lib.mapAttrs buildPackages build.envs
