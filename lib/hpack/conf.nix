{util}: let
  inherit (util) config lib;

  optionalField = name: conf: lib.optionalAttrs (lib.hasAttr name conf) { ${name} = conf.${name}; };

  mapComponents = f: lib.mapAttrs (name: comp: if name == "library" then f name comp else lib.mapAttrs f comp);

  mkPrelude = prelude: base: let
    mod = prelude.module;
    preludePackageBase = if lib.isAttrs prelude.package then prelude.package else { name = prelude.package; };
    preludePackage = preludePackageBase // {
      mixin = lib.optionals (mod != "Prelude") [
        "(${mod} as Prelude)"
        "hiding (${mod})"
      ];
    };
  in [preludePackage] ++ lib.optional (base != null) base;

  mkPathsBlocker = name:
  { when = { condition = false; generated-other-modules = "Paths_${lib.replaceStrings ["-"] ["_"] name}"; }; };

  replaceManagedBounds = managedBounds: resolving: dep: let
    norm = util.version.normalize dep;
    pkg = util.version.mainLibName norm.name;
    managed = util.version.normalizeManaged (managedBounds.${pkg} or null);

    version =
      # `resolving` means that the project is currently being built for a mutation, so we need to jailbreak everything.
      if resolving
      then ">=0"
      else
      # The flake bound should only be used when `mergeBounds` is enabled, irrespective of whether there's a defined
      # managed bound or not.
      if config.managed.mergeBounds
      then util.version.intersect norm.version managed
      else managed
      ;

  in { inherit (norm) name mixin; inherit version; };

  managedBounds = name: hconf: let
    state = util.managed.state.current;
    bounds = state.bounds.${name} or {};
  in
  { dependencies = map (replaceManagedBounds bounds state.resolving) hconf.dependencies or []; };

  componentWithManaged = name: hconf:
  hconf // managedBounds name hconf;

  componentGeneral = pkg: name: conf: let

    prelude = conf.prelude;
    base = conf.base;

    basic = { inherit (conf) ghc-options dependencies default-extensions language source-dirs; };

    # TODO omit base dep if it's already in conf.dependencies
    preludeDeps = {
      dependencies =
        if prelude.enable
        then mkPrelude prelude conf.baseHide
        else lib.optional (base != null) base;
    };

    paths = if conf.paths then {} else mkPathsBlocker pkg.name;

    full = util.mergeAll [
      preludeDeps
      basic
      paths
      conf.component
    ];

  in full;

  extraLibrary = mainLib: conf:
    lib.optionalAttrs (!mainLib && conf.public) { visibility = "public"; } //
    optionalField "reexported-modules" conf
    ;

  extraExecutable = conf: { inherit (conf) main; };

  plural = isLib: sort:
  if isLib
  then "internal-libraries"
  else "${sort}s";

  wrap = isLib: mainLib: conf: hconf:
  if mainLib
  then hconf
  else { ${plural isLib conf.internal.sort} = hconf; };

  componentFull = pkg: name: conf: let
    isLib = conf.internal.sort == "library";
    mainLib = isLib && name == "library";
    extra =
      if isLib
      then extraLibrary mainLib
      else extraExecutable
      ;
    comp = { ${name} = extra conf // componentGeneral pkg name conf; };
  in
  lib.optionalAttrs conf.enable (wrap isLib mainLib conf comp);

  packageComponents = pkg:
  util.mergeAll (lib.mapAttrsToList (componentFull pkg) pkg.internal.componentsSet);

  packageMeta = conf: let
    cabal = conf.cabal-config;

    basic = { inherit (conf) name; inherit (cabal) version; };

    optAttrs = [
      "author"
      "license"
      "license-file"
      "copyright"
      "build-type"
    ];

    opt = util.mapListCatAttrs (a: let v = cabal.${a}; in lib.optionalAttrs (v != null) { ${a} = v; }) optAttrs;

    desc = lib.optionalAttrs (conf.description != null) { inherit (conf) description; };
  in util.mergeAll [
    cabal.meta
    basic
    opt
    desc
  ];

  assemblePackages = meta: comps:
  lib.zipAttrsWith (_: util.mergeAll) [meta comps];

  meta = lib.mapAttrs (_: packageMeta) config.packages;

  components = lib.mapAttrs (_: packageComponents) config.packages;

  # TODO This needs some intermediate representation that is easier to traverse
  componentsWithManaged = lib.mapAttrs (pkg: mapComponents (_: componentWithManaged pkg)) (components);

  packages = assemblePackages meta components;

  # TODO I think this won't work if the user specified dependencies in freeform cabal options
  packagesWithManaged = assemblePackages meta componentsWithManaged;

in {
  inherit meta components packages packagesWithManaged;
}
