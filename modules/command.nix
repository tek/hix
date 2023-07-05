{global, util, ...}:
{name, config, lib, ...}:
with lib;
let

  envCommand = import ../lib/command.nix { config = global; inherit util; };

  cli = global.internal.hixCli.exe;

  json = util.json.ghciFile;

  ghciCommand = let
    staticRunner = if config.ghci.runner != null then "-r ${config.ghci.runner}" else "";
  in ''
  ghci_cmd=$(${cli} ghci-cmd --config ${json} ''${env_args[@]} ${staticRunner} ''${cmd_args[@]})
  eval $ghci_cmd
  '';

  ghcidCommand = ''
  ghcid_cmd=$(${cli} ghcid-cmd --config ${json} ''${env_args[@]} ''${cmd_args[@]})
  eval $ghcid_cmd
  '';

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

    ghci = {
      enable = mkOption {
        description = mdDoc ''
        Create a command that runs GHCi (like the built-in command) with some static options.
        For example, you can specify a [runner](#opt-ghci-ghci.run), and the app will be equivalent to running
        `nix run .#ghci -r <runner>`.
        '';
        type = bool;
        default = false;
      };

      ghcid = mkOption {
        description = mdDoc "Whether to run this command with GHCid instead of plain GHCi.";
        type = bool;
        default = false;
      };

      runner = mkOption {
        description = mdDoc "The name of a runner in [](#opt-ghci-ghci.run) and [](#opt-ghci-ghci.run).";
        type = nullOr string;
        default = null;
      };
    };

  };

  config = {

    path = (envCommand { command = config; env = global.envs.${config.env}; }).path;

    command = mkIf config.ghci.enable (if config.ghci.ghcid then ghcidCommand else ghciCommand);

  };
}
