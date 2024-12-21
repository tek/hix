{config, lib, ...}:
with lib;
let

  pkgs = config.pkgs;

  basic = import ./default.nix { inherit lib; };

  paramApp = import ./param-app.nix { inherit config lib util; };

  types = import ./types.nix { inherit config lib util; };

  hsLib = config.pkgs.haskell.lib;

  packageRel = basic.packageSubpath config.base;

  overridesDeps = name: config.internal.overridesDeps.${name} or [];

  overridesFromDeps = extra:
  basic.concatOverrides (map overridesDeps extra);

  overridesGlobal = extra: overridesFromDeps (["local" "all"] ++ extra);

  overridesGlobalMin = extra: overridesFromDeps (["localMin" "all"] ++ extra);

  projectHasPackages = !(basic.empty config.packages);

  withMainNameOr = alt: f:
  if projectHasPackages
  then f config.main
  else alt;

  withMainOr = alt: f: withMainNameOr alt (pkgName: f config.packages.${pkgName});

  attrsetMainName = withMainNameOr {};

  jsonFile = name: value: config.pkgs.writeText "hix-${name}-json" (builtins.toJSON value);

  json = let

    componentConf = c: {
      inherit (c) name language;
      extensions = c.default-extensions;
      ghcOptions = c.ghc-options;
      prelude = if c.prelude.enable then c.prelude else null;
      runner = if c.env == null then null else config.envs.${c.env}.runner;
      sourceDirs = c.source-dirs;
    };

    packageConf = pkg: {
      inherit (pkg) name;
      src = pkg.subpath;
      components = mapAttrs (_: componentConf) (util.internal.packages.normalized pkg);
    };

    packages = mapAttrs (_: packageConf) config.packages;

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

  in {
    inherit packages ghci preproc;

    envFile = default: jsonFile "env-config" (env default);

    ghciFile = jsonFile "ghci-config" ghci;

    preprocFile = jsonFile "preproc-config" preproc;

  };

  visibleEnvs = filterAttrs (_: e: !e.hide) config.envs;

  visibleAppEnvs = filterAttrs (_: e: !e.hideApps) config.envs;

  minGhcs = version:
  all (basic.minGhc version) (attrValues config.envs);

  minGhcDev = version:
  basic.minGhc version config.envs.dev;

  unlessDev = conf: v: mkIf (conf.name != "dev") (mkDefault v);

  scriptErr = name: text: pkgs.writeScript name ''
  #!${pkgs.runtimeShell}
  ${text}
  '';

  script = name: text: scriptErr name ''
  set -e
  ${text}
  '';

  zwrap = pkgs.writeScript "zsh-pure" ''
  #!/bin/sh
  exec ${pkgs.zsh}/bin/zsh -o no_global_rcs -o no_rcs $@
  '';

  zscriptPure = name: text: pkgs.writeScript name ''
  #!${zwrap}
  ${text}
  '';

  zscriptErr = name: text: pkgs.writeScript name ''
  #!${pkgs.zsh}/bin/zsh
  ${text}
  '';

  zscript = name: text: zscriptErr name ''
  setopt err_exit
  ${text}
  '';

  zapp = name: test: util.app (zscript name test);

  exportPath = packages: ''
  export PATH="${lib.makeBinPath packages}:$PATH"
  '';

  exportPathOptional = packages: lib.optional (!(basic.empty packages)) (exportPath packages);

  setupScript = {
    path ? [],
    console ? true,
    verbose ? true,
  }:
  basic.unlines (
    lib.optional console (basic.loadConsoleWith { inherit verbose; })
    ++
    exportPathOptional path
  );

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

  bootstrapWithStaticCli = name: pre: post: script name ''
  ${downloadStaticCli}
  ${pre}
  if ! git status &>/dev/null
  then
    ${pkgs.git}/bin/git init -q
  fi
  ${pkgs.git}/bin/git add .
  ${nixC} flake update --quiet --quiet
  ${pkgs.git}/bin/git add flake.lock
  ${post}
  '';

  bootstrapWithDynamicCli = name: pre: post: script name ''
  exe="${util.build.packages.min.hix.package}/bin/hix"
  ${pre}
  if ! ${pkgs.git}/bin/git status &>/dev/null
  then
    ${pkgs.git}/bin/git init -q
  fi
  ${pkgs.git}/bin/git add .
  ${nixC} flake update --quiet --quiet
  ${pkgs.git}/bin/git add flake.lock
  ${post}
  '';

  cacheWrapper = self: name: app: script name ''
  ${nixC} run ${self}#${app} -- "$@"
  '';

  envSystemAllowed = env:
  env.systems == null || (elem config.system env.systems);

  runBuildApp = name:
  "nix run .#${util.internalScope}.${name}";

  dummyAppScript = pre: sub: script "hix-dummy-app" ''
  ${basic.loadConsole}
  message "This app cannot be run, it is a namespace node with contents:"
  ${basic.unlinesMap (n: ''message_hang "$(yellow '*') $(blue .#${concatStringsSep "." (pre ++ [n])})"'') sub}
  '';

  dummyApp = pre: sub: basic.app (dummyAppScript pre sub);

  util = basic // {
    inherit
    config
    pkgs
    hsLib
    paramApp
    types
    packageRel
    overridesDeps
    overridesFromDeps
    overridesGlobal
    overridesGlobalMin
    projectHasPackages
    withMainNameOr
    withMainOr
    attrsetMainName
    jsonFile
    json
    visibleEnvs
    visibleAppEnvs
    minGhcs
    minGhcDev
    conf
    unlessDev
    scriptErr
    script
    zscriptPure
    zscriptErr
    zscript
    zapp
    exportPath
    exportPathOptional
    setupScript
    downloadStaticCli
    nixC
    bootstrapWithStaticCli
    bootstrapWithDynamicCli
    cacheWrapper
    envSystemAllowed
    runBuildApp
    dummyApp
    ;

    ghc = import ./ghc.nix { inherit config lib util; };

    ghcOverlay = import ./ghc-overlay.nix { inherit util; };

    managed = import ./managed/default.nix { inherit util; };

    package = import ./package.nix { inherit util; };

    command = import ./command.nix { inherit util; };

    hpack = import ./hpack/default.nix { inherit util; };

    internal = import ./internal/default.nix { inherit util; };

    build = import ./build/default.nix { inherit util; };

    outputs = import ./outputs/default.nix { inherit util; };
  };

in util
