{util}: let

  inherit (util) lib config internal project;

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
  then f mainPackage
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

  nonexistentMain = ''
  The option 'main' is set to '${config.main}', but no such package is defined.
  The available packages are: ${lib.concatStringsSep ", " (lib.attrNames config.packages)}
  '';

  mainPackage =
  if config.packages ? ${config.main}
  then config.packages.${config.main}
  else throw nonexistentMain;

  selectMain = pkgNames: let

    deps = lib.attrValues (util.restrictKeys pkgNames pkgsDeps);

    hasDepOn = target: pkg:
    pkg.name != target &&
    lib.elem target pkg.deps;

    isNoDep = pkg: !(lib.any (hasDepOn pkg) deps);

  in util.find isNoDep pkgNames;

  noSrcGeneral = pkgName: ''
  The package '${pkgName}' does not define the option 'src' in 'flake.nix'.
  This option should point to the root directory for this package, for example:

    packages = {
      ${pkgName}.src = ./packages/${pkgName};
      api.src = ./.;
    };
  '';

  noSrcMultiPackage = pkgName: ''
  ${noSrcGeneral pkgName}
  In projects with a single package, Hix uses the project root as the default for 'src', but this project defines multiple packages.
  '';

  noSrcNoBase = pkgName: ''
  ${noSrcGeneral pkgName}
  In projects with a single package, Hix uses the project root as the default for 'src'.
  This requires setting either of the options 'base' or 'self':

    outputs = {self, hix}: hix {
      inherit self;
      base = ./.;
      packages.${pkgName} = {};
    };
  '';

  srcDefault = pkgName:
    if !internal.project.singlePackage
    then throw (noSrcMultiPackage pkgName)
    else if project.specifiedBase == null
    then throw (noSrcNoBase pkgName)
    else project.specifiedBase;

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
  mainPackage
  withMain
  setWithMain
  withExe
  setWithExe
  normalized
  selectMain
  srcDefault
  map
  mapMaybe
  ;
}
