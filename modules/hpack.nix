{ lib, config, util, ... }:
with builtins;
with lib;
with types;
let

  libOutput = import ../lib/output.nix { inherit config lib util; };

  maybeDefaultApp = name: a:
  if name == config.defaultApp
  then { default = a; }
  else {};

  optionalField = name: conf: optionalAttrs (hasAttr name conf) { ${name} = conf.${name}; };

  appWithAppimage = pkg: name:
  util.app "${pkg}/bin/${name}" // libOutput.appimageApp "dev" name;

  appField = pkg: name: exe:
  let a = appWithAppimage pkg name;
  in { ${name} = a; } // maybeDefaultApp name a;

  packageApps = outputs: pname: conf: let
    main = libOutput.pkgMainExe config.packages.${pname};
    pkg = outputs.${pname};
  in
  optionalAttrs (main != null) { ${pname} = appWithAppimage pkg main.name; } //
  util.foldAttrs (mapAttrsToList (appField pkg) (conf.executables or {}))
  ;

  mkPrelude = prelude: base: let
    mod = prelude.module;
    preludePackageBase = if isAttrs prelude.package then prelude.package else { name = prelude.package; };
    preludePackage = preludePackageBase // {
      mixin = optionals (mod != "Prelude") [
        "(${mod} as Prelude)"
        "hiding (${mod})"
      ];
    };
  in [preludePackage] ++ optional (base != null) base;

  mkPathsBlocker = name:
  { when = [{ condition = false; generated-other-modules = "Paths_${replaceStrings ["-"] ["_"] name}"; }]; };

  # TODO normalize now unnecessary since it's done in cabalDep?
  replaceManagedDep = deps: dep: let
    norm = util.version.normalize dep;
    name = util.version.mainLibName norm.name;
  in if hasAttr name deps
  then norm // { inherit name; version = deps.${name}; }
  else dep;

  generateComponent = pkg: name: conf: let

    prelude = conf.prelude;
    base = conf.base;
    deps = attrByPath ["deps" pkg.name] {} util.managedDeps;

    basic = { inherit (conf) ghc-options dependencies default-extensions language source-dirs; };

    withoutBase = util.mergeAll [
      basic
      paths
      conf.component
    ];

    explicitBase = let
      isBase = dep: dep.name == "base";
    in any isBase withoutBase.dependencies or [];

    preludeDeps = {
      dependencies =
        if prelude.enable
        then mkPrelude prelude conf.baseHide
        else optional (base != null && !explicitBase) base;
    };

    paths = if conf.paths then {} else mkPathsBlocker pkg.name;

    full = util.mergeAll [
      preludeDeps
      withoutBase
    ];

    withManaged =
      if config.managedDeps.enable
      then util.mapDependencies (replaceManagedDep deps) full
      else full;

  in withManaged;

  extraLibrary = mainLib: conf:
    optionalAttrs (!mainLib && conf.public) { visibility = "public"; } //
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

  generateSort = pkg: name: conf: let
    sort = conf.internal.sort;
    isLib = sort == "library";
    mainLib = isLib && name == "library";
    extra =
      if isLib
      then extraLibrary mainLib
      else extraExecutable
      ;
  in
  optionalAttrs conf.enable (wrap isLib mainLib conf { ${name} = extra conf // generateComponent pkg name conf; });

  generatePackageConf = name: conf: let

    cabal = conf.cabal-config;

    basic = { inherit (conf) name; inherit (cabal) version; };

    optAttrs = [
      "author"
      "license"
      "license-file"
      "copyright"
      "build-type"
    ];

    opt = util.foldMapAttrs (a: let v = cabal.${a}; in optionalAttrs (v != null) { ${a} = v; }) optAttrs;

    desc = optionalAttrs (conf.description != null) { inherit (conf) description; };

    components = mapAttrsToList (generateSort conf) conf.internal.componentsSet;

    merged = util.mergeAll ([
      cabal.meta
      basic
      opt
      desc
    ] ++ components);

  in util.mapComponents (util.mapDependencies util.version.normalize) merged;

  removeCondition = dep: removeAttrs dep ["condition"];

  # TODO generalize for non-dependencies
  # TODO can be grouped by condition
  renderCondition = condition: dep: let
    rendered =
      if condition.type == "verbatim"
      then condition.args.value
      else if lib.hasAttr condition.type config.conditions
      then (config.conditions.${condition.type} condition.args).render
      else throw "Invalid condition type '${condition.type}': Not defined in 'config.conditions'.";
  in { condition = rendered; dependencies = [dep]; };

  processCondition = dep:
  renderCondition dep.condition (removeCondition dep);

  conditionalDeps = deps: let
    conds = map processCondition deps;
  in optionalAttrs (conds != []) { when = conds; };

  processConditions = conf: let
    byCondition = lib.partition (dep: dep.condition == null) conf.dependencies or [];
  in util.mergeAll [
    (conf // { dependencies = map removeCondition byCondition.right; })
    (conditionalDeps byCondition.wrong)
  ];

  script = import ../lib/hpack.nix { inherit config; verbose = true; };

  scriptQuiet = import ../lib/hpack.nix { inherit config; };

in {
  options = {

    defaultApp = mkOption {
      type = str;
      description = mdDoc ''
      The name of an executable in [](#opt-general-packages) that should be assigned to `packages.default`.
      '';
    };

    hpack = {

      apps = mkOption {
        type = lazyAttrsOf util.types.flakeApp;
        default = {};
      };

      script = mkOption {
        type = path;
        description = mdDoc ''
          The script that generates a Cabal file in each of the directories configured in `packages` by
          executing `hpack`.
          It is intended to be run manually in the package directory using `nix run .#hpack`.
          If `hpack.packages` is defined, it is used to synthesize a `package.yaml`.
          Otherwise, the file needs to be present in the source directory.
        '';
      };

      scriptQuiet = mkOption {
        type = path;
        description = mdDoc ''
          Same as `script`, but suppress all output.
        '';
      };

      internal.packages = mkOption {
        type = attrsOf util.types.strict;
      };

      internal.finalPackages = mkOption {
        type = attrsOf util.types.strict;
      };
    };
  };

  config = {

    defaultApp = mkDefault config.main;

    hpack = {

      apps = mkDefault (
        util.foldAttrs (mapAttrsToList (packageApps config.envs.dev.derivations) config.hpack.internal.packages)
      );

      script = script;

      scriptQuiet = scriptQuiet;

      internal.packages = mapAttrs generatePackageConf config.packages;

      internal.finalPackages = mapAttrs (_: util.mapComponents processConditions) config.hpack.internal.packages;

    };
  };
}
