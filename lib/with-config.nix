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

in basic // {
  inherit
  paramApp
  types
  packageRel
  overridesGlobal
  overridesGlobalMin
  ;
}
