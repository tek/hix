{ config, lib, util, ... }:
with lib;
let

  pkgs = config.pkgs;

  basic = import ./default.nix { inherit lib; };

  paramApp = import ./param-app.nix { inherit config lib util; };

  types = import ./types.nix { inherit config lib; };

  packageRel = util.packageSubpath config.base;

  overridesDeps = name: config.internal.overridesDeps.${name} or [];

  overridesFromDeps = extra:
  util.concatOverrides (map overridesDeps extra);

  overridesGlobal = extra:
  util.concatOverrides [
    (overridesFromDeps (["local" "all"] ++ extra))
    config.overrides
  ];

  overridesGlobalMin = extra:
  util.concatOverrides [
    (overridesFromDeps (["localMin" "all"] ++ extra))
    config.overrides
  ];

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

    env = default: {
      inherit packages;
      defaultEnv = default.runner;
    };

    # TODO add to this set:
    # - component-dependent ghci args
    # - restarts
    # - cwd
    ghci = {
      inherit packages;
      setup = config.ghci.setup;
      run = config.ghci.run;
      args = config.ghci.args;
    };

    preproc = {
      packages = if config.manualCabal then null else packages;
    };

    jsonFile = name: value: pkgs.writeText "hix-${name}-json" (builtins.toJSON value);

  in {
    inherit packages ghci preproc;

    envFile = default: jsonFile "env-config" (env default);

    ghciFile = jsonFile "ghci-config" ghci;

    preprocFile = jsonFile "preproc-config" preproc;

  };

  visibleEnvs = filterAttrs (_: e: !e.hide) config.envs;

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

  withStaticCLI = name: script: pkgs.writeScript name ''
  #!${pkgs.bashInteractive}/bin/bash
  set -e
  ${downloadStaticCli}
  ${script}
  if [[ ! -e .git ]]
  then
    ${pkgs.git}/bin/git init
  fi
  ${pkgs.git}/bin/git add .
  ${pkgs.nix}/bin/nix flake update
  ${pkgs.git}/bin/git add flake.lock
  '';

in basic // {
  inherit
  paramApp
  types
  packageRel
  overridesDeps
  overridesGlobal
  overridesGlobalMin
  json
  visibleEnvs
  minGhcs
  conf
  unlessDev
  downloadStaticCli
  withStaticCLI
  ;
}
