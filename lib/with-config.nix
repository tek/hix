{config, lib, ...}:
with lib;
let

  pkgs = config.pkgs;

  basic = import ./default.nix { inherit lib; };

  paramApp = import ./param-app.nix { inherit config lib util; };

  types = import ./types.nix { inherit config lib; };

  hsLib = config.pkgs.haskell.lib;

  managedEnv = let
    file = "${config.base}/${config.managed.file}";
  in optionalAttrs (config.managed.enable && pathExists file) (import file);

  packageRel = basic.packageSubpath config.base;

  overridesDeps = name: config.internal.overridesDeps.${name} or [];

  overridesFromDeps = extra:
  basic.concatOverrides (map overridesDeps extra);

  overridesGlobal = extra: overridesFromDeps (["local" "all"] ++ extra);

  overridesGlobalMin = extra: overridesFromDeps (["localMin" "all"] ++ extra);

  projectHasPackages = !(basic.empty config.packages);

  withMainOr = fallback: f:
  if projectHasPackages
  then f config.main
  else fallback;

  attrsetMain = withMainOr {};

  json = let

    inherit (config) pkgs;

    componentConf = c: {
      inherit (c) name language;
      extensions = c.default-extensions;
      ghcOptions = c.ghc-options;
      prelude = if c.prelude.enable then c.prelude else null;
      runner = if c.env == null then null else config.envs.${c.env}.runner;
      sourceDirs = c.source-dirs;
    };

    packageConf = p: {
      inherit (p) name;
      src = p.subpath;
      components = mapAttrs (_: componentConf) p.internal.componentsSet;
    };

    packages = mapAttrs (_: packageConf) config.packages;

    packageDeps = _: pkg:
    mapAttrs (_: c: { dependencies = map basic.version.normalize c.dependencies; }) pkg.internal.componentsSet;

    env = default: {
      mainPackage = config.main;
      inherit packages;
      defaultEnv = default.runner;
    };

    # TODO add to this set:
    # - component-dependent ghci args
    # - restarts
    # - cwd
    ghci = {
      mainPackage = config.main;
      inherit packages;
      setup = config.ghci.setup;
      run = config.ghci.run;
      args = config.ghci.args;
    };

    preproc = {
      packages = if config.manualCabal then null else packages;
    };

    managed = targets: {
      deps = mapAttrs packageDeps config.packages;
      state = managedEnv;
      lower = {
        inherit (config.managed.lower) solverBounds;
      };
      inherit targets;
    };

    jsonFile = name: value: pkgs.writeText "hix-${name}-json" (builtins.toJSON value);

  in {
    inherit packages ghci preproc;

    envFile = default: jsonFile "env-config" (env default);

    ghciFile = jsonFile "ghci-config" ghci;

    preprocFile = jsonFile "preproc-config" preproc;

    managedFile = targets: jsonFile "managed-config" (managed targets);

  };

  visibleEnvs = filterAttrs (_: e: !e.hide) config.envs;

  visibleAppEnvs = filterAttrs (_: e: !e.hideApps) config.envs;

  minGhcs = version:
  all (basic.minGhc version) (attrValues config.envs);

  unlessDev = conf: v: mkIf (conf.name != "dev") (mkDefault v);

  downloadStaticCli = ''
  tmp=$(mktemp -d)
  quit()
  {
    rm -rf $tmp
  }
  trap quit EXIT
  exe="$tmp/hix"
  ${config.pkgs.curl}/bin/curl --no-progress-meter --location --output $exe ${config.internal.hixCli.staticExeUrl}
  chmod +x $exe
  '';

  nixC = "${config.pkgs.nix}/bin/nix --option extra-substituters 'https://tek.cachix.org' --option extra-trusted-public-keys 'tek.cachix.org-1:+sdc73WFq8aEKnrVv5j/kuhmnW2hQJuqdPJF5SnaCBk='";

  bootstrapWithStaticCli = name: pre: post: pkgs.writeScript name ''
  #!${pkgs.bashInteractive}/bin/bash
  set -e
  ${downloadStaticCli}
  ${pre}
  if ! git status &>/dev/null
  then
    ${pkgs.git}/bin/git init
  fi
  ${pkgs.git}/bin/git add .
  ${nixC} flake update --quiet --quiet
  ${pkgs.git}/bin/git add flake.lock
  ${post}
  '';

  bootstrapWithDynamicCli = name: pre: post: pkgs.writeScript name ''
  #!${pkgs.bashInteractive}/bin/bash
  set -e
  exe="${config.outputs.packages.hix}/bin/hix"
  ${pre}
  if ! git status &>/dev/null
  then
    ${pkgs.git}/bin/git init
  fi
  ${pkgs.git}/bin/git add .
  ${nixC} flake update --quiet --quiet
  ${pkgs.git}/bin/git add flake.lock
  ${post}
  '';

  cacheWrapper = self: name: app: pkgs.writeScript name ''
  ${nixC} run ${self}#${app} -- "$@"
  '';

  envSystemAllowed = env:
  env.systems == null || (elem config.system env.systems);

  runBuildApp = name:
  "nix run .#${config.buildOutputsPrefix}.${name}";

  util = basic // {
    inherit
    config
    hsLib
    paramApp
    types
    managedEnv
    packageRel
    overridesDeps
    overridesFromDeps
    overridesGlobal
    overridesGlobalMin
    projectHasPackages
    withMainOr
    attrsetMain
    json
    visibleEnvs
    visibleAppEnvs
    minGhcs
    conf
    unlessDev
    downloadStaticCli
    nixC
    bootstrapWithStaticCli
    bootstrapWithDynamicCli
    cacheWrapper
    envSystemAllowed
    runBuildApp
    ;

    ghc = import ./ghc.nix { inherit config lib util; };

    managed = import ./managed.nix { inherit util; };

    env = import ./env.nix { inherit util; };
  };

in util
