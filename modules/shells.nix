{config, lib, util, ...}:
with lib;
let

  shellModule = import ./shell-mod.nix { global = config; inherit util; };

in {
  options = with types; {

    shells = mkOption {
      description = mdDoc "Shells???";
      type = attrsOf (submodule shellModule);
      default = {};
    };
      
  };
}
