{global, util, ...}:
{name, config, lib, ...}:
with lib;
let

  inherit (global) pkgs;

  envModule = import ./env.nix { inherit global util; };

  envCommand = import ../lib/command.nix { config = global; inherit util; };

in {
  options = with types; {

    name = mkOption {
      description = mdDoc "Name";
      type = str;
      default = name;
    };

    env = mkOption {
      description = mdDoc "The default env for the command.";
      type = util.types.env;
      default = "dev";
    };

    command = mkOption {
      description = mdDoc "The script executed by this command.";
      type = str;
    };

    component = mkOption {
      description = mdDoc ''
      Whether this command should determine the env based on a target component specified by command line arguments.
      '';
      type = bool;
      default = false;
    };

    path = mkOption {
      description = mdDoc "The final executable.";
      type = path;
      readOnly = true;
    };

  };

  config = {

    path = (envCommand { command = config; env = global.envs.${config.env}; }).path;

  };
}
