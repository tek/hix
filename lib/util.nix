{config, lib, extra ? {}, ...}:
let

  basic = import ./default.nix { inherit lib; };

  paramApp = import ./param-app.nix { inherit config lib util; };

  types = import ./types.nix { inherit config lib util; };

  build = import ./build/default.nix { inherit util; };

  pkgs = build.nixpkgs.internal.pkgs;

  hsLib = pkgs.haskell.lib;

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

  jsonFile = name: value: pkgs.writeText "hix-${name}.json" (builtins.toJSON value);

  minGhcDev = version:
  basic.minGhc version config.envs.dev;

  unlessDevEnv = conf: v: lib.mkIf (conf.name != "dev") (lib.mkDefault v);

  unlessDevGhc = conf: v: lib.mkIf (conf.env != "dev") (lib.mkDefault v);

  scriptErr = name: text: pkgs.writeScript name ''
  #!${pkgs.runtimeShell}
  ${text}
  '';

  script = name: text: scriptErr name ''
  set -e
  ${text}
  '';

  scriptErrBin = name: text: pkgs.writeScriptBin name ''
  #!${pkgs.runtimeShell}
  ${text}
  '';

  scriptBin = name: text: scriptErrBin name ''
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

  zscriptErrBin = name: text: pkgs.writeScriptBin name ''
  #!${pkgs.zsh}/bin/zsh
  ${text}
  '';

  zscriptBin = name: text: zscriptErrBin name ''
  setopt err_exit
  ${text}
  '';

  zapp = name: test: util.app (zscript name test);

  nixWrapper = scriptBin "nix" ''
  if [[ -n ''${hix_nix_quiet:-} ]]
  then
    exec ${pkgs.nix}/bin/nix --quiet --quiet --quiet "$@"
  else
    exec ${pkgs.nix}/bin/nix "$@"
  fi
  '';

  exportPath = packages: ''
  export PATH="${lib.makeBinPath packages}:$PATH"
  '';

  exportPathOptional = packages: lib.optional (!(basic.empty packages)) (exportPath packages);

  mkPackages = spec:
  if lib.isFunction spec
  then spec pkgs
  else spec;

  setupScript = {
    path ? [],
    console ? true,
    verbose ? true,
    nix ? false,
  }: let
    extraPath = lib.optional nix nixWrapper;
  in
  basic.unlines (
    lib.optional console (basic.loadConsoleWith { inherit verbose; })
    ++
    exportPathOptional (extraPath ++ mkPackages path)
  );

  setupScriptFile = conf: pkgs.writeText "hix-setup-script" (setupScript conf);

  hixScript = name: conf: main: zscript name ''
  source ${setupScriptFile conf}
  ${main}
  '';

  downloadStaticCli = ''
  tmp=$(mktemp -d)
  quit()
  {
    rm -rf $tmp
  }
  trap quit EXIT
  exe="$tmp/hix"
  ${pkgs.curl}/bin/curl --no-progress-meter --location --output $exe ${config.internal.hixCli.staticExeUrl}
  chmod +x $exe
  '';

  nixC = "nix --option extra-substituters 'https://tek.cachix.org' --option extra-trusted-public-keys 'tek.cachix.org-1:+sdc73WFq8aEKnrVv5j/kuhmnW2hQJuqdPJF5SnaCBk='";

  bootstrapWithStaticCli = name: cmd: script name ''
  ${setupScript { path = [pkgs.git]; nix = true; }}
  ${downloadStaticCli}
  ${cmd}
  '';

  bootstrapWithDynamicCli = name: cmd: script name ''
  ${setupScript { path = [pkgs.git]; nix = true; }}
  exe="${util.build.packages.min.hix.package}/bin/hix"
  ${cmd}
  '';

  envSystemAllowed = env:
  env.systems == null || (lib.elem config.system env.systems);

  runBuildApp = name:
  "nix --quiet --quiet --quiet --show-trace run .#${util.internalScope}.${name}";

  legacyApp = package: exe:
  package // { meta.mainProgram = exe; };

  legacyAppScript = name: cmdline:
  util.zscriptErrBin name ''
  exec ${cmdline} $*
  '';

  ensureLegacyApp = name: exe:
  if exe ? meta.mainProgram
  then exe
  else legacyAppScript name exe;

  ensureLegacyApps = lib.mapAttrs ensureLegacyApp;

  exposeDefault = {
    appimage = true;
    internals = true;
    managed = true;
    cross = true;
    static = true;
  };

  expose = exposeDefault // util.config.output.expose;

  util = basic // extra // {
    inherit
    config
    pkgs
    build
    hsLib
    paramApp
    types
    overridesDeps
    overridesFromDeps
    overridesGlobal
    overridesGlobalMin
    projectHasPackages
    withMainNameOr
    withMainOr
    attrsetMainName
    jsonFile
    minGhcDev
    unlessDevEnv
    unlessDevGhc
    scriptErr
    script
    scriptBin
    scriptErrBin
    zscriptPure
    zscriptErr
    zscript
    zscriptErrBin
    zscriptBin
    zapp
    exportPath
    exportPathOptional
    setupScript
    hixScript
    downloadStaticCli
    nixC
    bootstrapWithStaticCli
    bootstrapWithDynamicCli
    envSystemAllowed
    runBuildApp
    legacyApp
    legacyAppScript
    ensureLegacyApp
    ensureLegacyApps
    expose
    ;

    path = import ./path.nix { inherit util; };

    managed = import ./managed/default.nix { inherit util; };

    hpack = import ./hpack/default.nix { inherit util; };

    oc = import ./deps/spec.nix { inherit lib; };

    internal = import ./internal/default.nix { inherit util; };

    project = import ./project/default.nix { inherit util; };

    outputs = import ./outputs/default.nix { inherit util; };
  };

in util
