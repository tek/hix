{ config, lib, util, ... }:
with lib;
let

  basic = import ./default.nix { inherit lib; };

  paramApp = import ./param-app.nix { inherit config lib util; };

  types = import ./types.nix { inherit lib; };

  packageRel = util.packageSubpath config.base;

in basic // {
  inherit
  paramApp
  types
  packageRel
  ;
}
