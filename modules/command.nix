{global, util, ...}:
{name, config, lib, ...}: let

  inherit (lib) types;

in {

  options = {

    name = lib.mkOption {
      description = "Name";
      type = types.str;
      default = name;
    };

    env = lib.mkOption {
      description = "The default env for the command.";
      type = util.types.env;
      default = "dev";
    };

    command = lib.mkOption {
      description = "The script executed by this command.";
      type = types.str;
    };

    component = lib.mkOption {
      description = ''
      Whether this command should determine the env based on a target component specified by command line arguments.

      ::: {.note}
      The component selector chooses a default component when no arguments are given.
      If that component has an explicit environment configured, it will be used instead of the one configured in this
      command.
      :::
      '';
      type = types.bool;
      default = false;
    };

    expose = lib.mkOption {
      description = "Whether this command should be a top-level flake app.";
      type = types.bool;
      default = false;
    };

    ghci = {
      enable = lib.mkOption {
        description = ''
        Create a command that runs GHCi (like the built-in command) with some static options.
        For example, you can specify a [runner](#opt-ghci-ghci.run), and the app will be equivalent to running
        `nix run .#ghci -r <runner>`.
        '';
        type = types.bool;
        default = false;
      };

      ghcid = lib.mkOption {
        description = "Whether to run this command with GHCid instead of plain GHCi.";
        type = types.bool;
        default = false;
      };

      runner = lib.mkOption {
        description = "The name of a runner in [](#opt-ghci-ghci.run) and [](#opt-ghci-ghci.run).";
        type = types.nullOr types.str;
        default = null;
      };

      package = lib.mkOption {
        description = "The name of the package passed to the GHCi runner with `-p`.";
        type = types.nullOr types.str;
        default = null;
      };

      module = lib.mkOption {
        description = "The name of the module passed to the GHCi runner with `-m`.";
        type = types.nullOr types.str;
        default = null;
      };

      component = lib.mkOption {
        description = "The name of the component passed to the GHCi runner with `-c`.";
        type = types.nullOr types.str;
        default = null;
      };

    };

  };

  config = {

    command =
      lib.mkIf config.ghci.enable
      (util.command.ghciCommand.extend (_: super: { config = super.config // config.ghci; })).script;

  };
}
