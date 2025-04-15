{util}: let

  inherit (util) lib config internal;

  maybeDefaultApp = name: a:
  if name == config.defaultApp
  then { default = a; }
  else {};

  exeApp = pkg: name:
  util.app "${pkg}/bin/${name}";

  appField = env: pkg: name: exe:
  let a = exeApp pkg name;
  in { ${name} = a; } // maybeDefaultApp name a;

  packageApps = env: outputs: pname: conf: let
    main = internal.package.mainExe config.packages.${pname};
    pkg = outputs.${pname};
  in
  lib.optionalAttrs (main != null) { ${pname} = exeApp pkg main.name; } //
  util.catSets (lib.mapAttrsToList (appField env pkg) (conf.executables or {}))
  ;

  withMain = alt: f:
  if util.projectHasPackages
  then f config.packages.${config.main}
  else alt;

  setWithMain = withMain {};

  withExe = alt: f: withMain alt (internal.packages.withExe alt f);

  setWithExe = withExe {};

  normalizedMulti = pkg: let
    withNames = util.mapKeys (_: comp: comp.name);

    enabled = lib.filterAttrs (_: comp: comp.enable);

    multi = sort: withNames (enabled pkg.${sort});
  in
    multi "libraries" //
    multi "executables" //
    multi "tests" //
    multi "benchmarks"
    ;

  normalized = pkg:
    lib.optionalAttrs pkg.library.enable { library = pkg.library; } //
    lib.optionalAttrs pkg.executable.enable { ${pkg.executable.name} = pkg.executable; } //
    lib.optionalAttrs pkg.test.enable { ${pkg.test.name} = pkg.test; } //
    lib.optionalAttrs pkg.benchmark.enable { ${pkg.benchmark.name} = pkg.benchmark; } //
    normalizedMulti pkg
    ;

  pkgDeps = pkg:
  lib.concatMap (c: lib.map util.cabalDepPackage c.dependencies) (lib.attrValues (normalized pkg));

  pkgsDeps = util.mapValues (pkg: { inherit (pkg) name; deps = pkgDeps pkg; }) config.packages;

  selectMain = pkgNames: let

    deps = lib.attrValues (util.restrictKeys pkgNames pkgsDeps);

    hasDepOn = target: pkg:
    pkg.name != target &&
    lib.elem target pkg.deps;

    isNoDep = pkg: !(lib.any (hasDepOn pkg) deps);

  in util.find isNoDep pkgNames;

  mapPkg = f: pkgName: a: f (util.justAttr pkgName config.packages) a;

  # map ::
  #   (Maybe Package -> a -> b) ->
  #   Map PackageName a ->
  #   Map PackageName b
  map = f: lib.mapAttrs (mapPkg f);

  # mapMaybe ::
  #   (Maybe Package -> a -> Maybe b) ->
  #   Map PackageName a ->
  #   Map PackageName b
  mapMaybe = f: util.mapMaybe (mapPkg f);

in {
  inherit
  packageApps
  withMain
  setWithMain
  withExe
  setWithExe
  normalized
  selectMain
  map
  mapMaybe
  ;
}
