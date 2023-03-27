{config, lib, util, ...}:
with lib;
let

  commandModule = import ./command.nix { global = config; inherit util; };

in {
  options = with types; {

    commands = mkOption {
      description = "";
      type = attrsOf (submodule commandModule);
      default = {};
    };

  };

  config = {

  };
}
