{config, lib, util, ...}:
with lib;
let
  envModule = import ./env.nix { global = config; inherit util; };
in {
  options = with types; {

    envs = mkOption {
      description = "";
      type = attrsOf (submodule envModule);
      default = {};
    };

    defaultEnv = mkOption {
      description = "";
      type = submodule envModule;
    };

  };

  config = {
    defaultEnv = {
      ghcid = mkDefault true;
      hls = mkDefault true;
    };
  };
}
