{ config, lib, util, ... }:
with lib;
let

  basic = import ./default.nix { inherit lib; };

  paramApp = import ./param-app.nix { inherit config lib util; };

  types = import ./types.nix { inherit config lib; };

  packageRel = util.packageSubpath config.base;

  overridesDeps = name: config.internal.overridesDeps.${name} or [];

  overridesFromDeps = extra:
  util.concatOverrides ([(overridesDeps "all")] ++ map overridesDeps extra);

  overridesGlobal = extra:
  util.concatOverrides [
    (overridesFromDeps (["local"] ++ extra))
    config.overrides
  ];

  overridesGlobalMin = extra:
  util.concatOverrides [
    (overridesFromDeps (["localMin"] ++ extra))
    config.overrides
  ];

  json = let

    inherit (config) pkgs;

    componentConf = c: {
      inherit (c) name;
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

    jsonFile = name: value: pkgs.writeText "hix-${name}-json" (builtins.toJSON value);

  in {
    inherit packages ghci;

    envFile = default: jsonFile "env-config" (env default);

    ghciFile = jsonFile "ghci-config" ghci;

  };

  visibleEnvs = filterAttrs (_: e: !e.hide) config.envs;

  minGhcs = version:
  all (basic.minGhc version) (attrValues config.envs);

in basic // {
  inherit
  paramApp
  types
  packageRel
  overridesGlobal
  overridesGlobalMin
  json
  visibleEnvs
  minGhcs
  ;
}
