{global, util, ...}:
{name, config, lib, ...}:
with lib;
let

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

      ::: {.note}
      The component selector chooses a default component when no arguments are given.
      If that component has an explicit environment configured, it will be used instead of the one configured in this
      command.
      :::
      '';
      type = bool;
      default = false;
    };

    expose = mkOption {
      description = mdDoc "Whether this command should be a top-level flake app.";
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
