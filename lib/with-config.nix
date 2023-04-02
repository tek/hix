{ config, lib, util, ... }:
with lib;
let

  basic = import ./default.nix { inherit lib; };

  paramApp = import ./param-app.nix { inherit config lib util; };

  types = import ./types.nix { inherit lib; };

  packageRel = util.packageSubpath config.base;

  overridesDeps = name: config.internal.overridesDeps.${name} or [];

  overridesFromDeps = extra:
  util.concatOverrides ([(overridesDeps "all")] ++ map overridesDeps extra);

  overridesGlobal = extra:
  util.concatOverrides [
    (overridesFromDeps (["local"] ++ extra))
    config.overrides
    config.internal.overridesLocal
  ];

  overridesGlobalMin = extra:
  util.concatOverrides [
    (overridesFromDeps (["localMin"] ++ extra))
    config.overrides
    config.internal.overridesLocalMin
  ];

  json = let

    inherit (config) pkgs;

    componentConf = c: {
      inherit (c) name;
      inherit (c.env) runner;
      sourceDirs = c.source-dirs;
    };

    packageConf = p: {
      inherit (p) name;
      src = p.subpath;
      components = mapAttrs (_: componentConf) p.componentsSet;
    };

    packages = mapAttrs (_: packageConf) config.packages;

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

    packagesFile = jsonFile "packages-config" packages;

    ghciFile = jsonFile "ghci-config" ghci;

  };

in basic // {
  inherit
  paramApp
  types
  packageRel
  overridesGlobal
  overridesGlobalMin
  json
  ;
}
