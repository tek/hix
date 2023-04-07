{ lib, config, util, ... }:
with builtins;
with lib;
with types;
let

  maybeDefaultApp = name: a:
  if name == config.defaultApp
  then { default = a; }
  else {};

  app = pkg: name: exe:
  let a = { type = "app"; program = "${pkg}/bin/${name}"; };
  in { ${name} = a; } // maybeDefaultApp name a;

  packageApps = outputs: pname: conf:
  util.foldAttrs (mapAttrsToList (app (outputs.${pname})) (conf.executables or {}));

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
  { when = { condition = false; generated-other-modules = "Paths_${replaceStrings ["-"] ["_"] name}"; }; };

  generateComponent = pkg: conf: let

    prelude = conf.prelude;
    base = conf.base;

    basic = { inherit (conf) ghc-options dependencies default-extensions language source-dirs; };

    preludeDeps = {
      dependencies =
        if prelude.enable
        then mkPrelude prelude conf.baseHide
        else optional (base != null) base;
    };

    paths = if conf.paths then {} else mkPathsBlocker pkg.name;

  in util.mergeAll [
    preludeDeps
    basic
    paths
    conf.component
  ];

  generateLib = pkg: conf:
  { library = generateComponent pkg conf; };

  generateExe = pkg: conf:
  { ${conf.name} = { inherit (conf) main; } // generateComponent pkg conf; };

  generateExes = name: conf: let
    plural = "${name}s";
    exes = optional conf.${name}.enable conf.${name} ++ attrValues conf.${plural};
  in { ${plural} = util.foldMapAttrs (generateExe conf) exes; };

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

    library = optionalAttrs conf.library.enable (generateLib conf conf.library);
    exes = generateExes "executable" conf;
    tests = generateExes "test" conf;
    benches = generateExes "benchmark" conf;

  in util.mergeAll [
    cabal.meta
    basic
    opt
    desc
    library
    exes
    tests
    benches
  ];

  infer = mapAttrs generatePackageConf config.packages;

  script = import ../lib/hpack.nix { inherit config; verbose = true; };

  scriptQuiet = import ../lib/hpack.nix { inherit config; };

in {
  options = {

    defaultApp = mkOption {
      type = str;
      description = mdDoc ''
      The name of an executable in {option}`packages` that should be assigned to
      `packages.default`.
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

      internal = {

        packages = mkOption {
          type = attrsOf unspecified;
        };

      };
    };
  };

  config = {

    defaultApp = mkDefault config.main;

    hpack = {

      internal.packages = mapAttrs generatePackageConf config.packages;

      apps = mkDefault (
        util.foldAttrs (mapAttrsToList (packageApps config.envs.dev.derivations) config.hpack.internal.packages)
      );

      script = script;

      scriptQuiet = scriptQuiet;

    };
  };
}
