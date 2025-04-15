{util}: let
  inherit (util) config lib internal;

  optionalField = name: conf: lib.optionalAttrs (lib.hasAttr name conf) { ${name} = conf.${name}; };

  normalizeDeps = c: c // { dependencies = map util.version.normalize c.dependencies or []; };

  mkPrelude = prelude: base: let
    mod = prelude.module;
    preludePackageBase = if lib.isAttrs prelude.package then prelude.package else { name = prelude.package; };
    preludePackage = preludePackageBase // {
      mixin = lib.optionals (mod != "Prelude") [
        "(${mod} as Prelude)"
        "hiding (${mod})"
      ];
    };
  in [preludePackage] ++ lib.optional (base != null && preludePackage.name != "base") base;

  mkPathsBlocker = name:
  { when = { condition = false; generated-other-modules = "Paths_${lib.replaceStrings ["-"] ["_"] name}"; }; };

  addForcedBounds = pkg: managed: let
    forced = config.managed.forceBounds.${pkg} or null;
  in
  if managed == null || !(lib.isAttrs managed)
  then forced
  else if forced == null
  then managed
  else {
    lower = if forced.lower == null then managed.lower or null else forced.lower;
    upper = if forced.upper == null then managed.upper or null else forced.upper;
  };

  # Hackage will refuse uploads if base doesn't have an upper bound.
  # This is particularly problematic for tests.
  ensureBaseBound = pkg: version: let

    withBound = v: v // { upper = "5"; };

  in
  if pkg != "base"
  then version
  else if version == null
  then withBound { lower = null; }
  else if version ? lower && version.lower == null
  then withBound version
  else version;

  replaceManagedBounds = managedBounds: resolving: dep: let
    norm = util.version.normalize dep;
    pkg = util.version.mainLibName norm.name;
    managed = addForcedBounds pkg (managedBounds.${pkg} or null);
    managedNorm = util.version.normalizeManaged (ensureBaseBound pkg managed);

    # If the managed version of `base` is `null`, we don't want to intersect the amended bounds with the configured
    # version, since that will result in something like `(>= 4 && < 5) && (<5)`.
    intersected =
      if norm.version == null
      then managedNorm
      else if managed == null
      then norm.version
      else util.version.intersect norm.version managedNorm;

    version =
      # `resolving` means that the project is currently being built for a managed dependency mutation, so we need to
      # jailbreak everything.
      if resolving
      then ">=0"
      else
      # The flake bound should only be used when `mergeBounds` is enabled, irrespective of whether there's a defined
      # managed bound or not.
      if config.managed.mergeBounds
      then intersected
      else
      # We only want managed bounds, but local packages need manual versions
      if managedNorm == null && lib.isAttrs dep && dep.local or false
      then norm.version
      else managedNorm
      ;

  in { inherit (norm) name mixin; inherit version; };

  managedBounds = name: hconf: let
    state = util.managed.state.current;
    bounds = state.bounds.${name} or {};
  in
  { dependencies = map (replaceManagedBounds bounds state.resolving) hconf.dependencies or []; };

  # --------------------------------------------------------------------------------------------------------------------

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

    full = util.mergeAllAttrs [
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

  componentFull = pkg: name: conf: let
    isLib = conf.internal.sort == "library";
    mainLib = isLib && conf.internal.single;
    extra =
      if isLib
      then extraLibrary mainLib
      else extraExecutable
      ;
    comp = extra conf // componentGeneral pkg name conf // { __conf = conf; };
  in
  lib.optionalAttrs conf.enable comp;

  # --------------------------------------------------------------------------------------------------------------------

  packageComponents = pkg:
  lib.mapAttrs (componentFull pkg) (internal.packages.normalized pkg);

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
  in util.mergeAllAttrs [
    cabal.meta
    basic
    opt
    desc
  ];

  # --------------------------------------------------------------------------------------------------------------------

  plural = isLib: sort:
  if isLib
  then "internal-libraries"
  else "${sort}s";

  wrap = isLib: mainLib: conf: comp:
  if mainLib
  then comp
  else { ${plural isLib conf.internal.sort} = comp; };

  hpackComponent = name: comp: let
    conf = comp.__conf;
    isLib = conf.internal.sort == "library";
    mainLib = isLib && conf.internal.single;
  in wrap isLib mainLib conf { ${name} = comp; };

  hpackProto = pkg: util.mergeAllAttrs (lib.mapAttrsToList hpackComponent pkg);

  # --------------------------------------------------------------------------------------------------------------------

  meta = util.mapValues packageMeta config.packages;

  assemblePackages = comps:
  lib.zipAttrsWith (_: util.mergeAllAttrs) [meta comps];

  componentsRaw = util.mapValues packageComponents config.packages;

  normalized = {

    components = util.mapValues (util.mapValues normalizeDeps) componentsRaw;

    componentsWithManaged = lib.mapAttrs (name: util.mapValues (componentWithManaged name)) componentsRaw;

  };

  components = util.mapValues hpackProto normalized.components;

  componentsWithManaged = util.mapValues hpackProto normalized.componentsWithManaged;

  packages = assemblePackages components;

  # TODO I think this won't work if the user specified dependencies in freeform cabal options
  packagesWithManaged = assemblePackages componentsWithManaged;

in {

  inherit meta normalized components componentsWithManaged packages packagesWithManaged;

}
