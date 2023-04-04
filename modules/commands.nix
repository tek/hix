{config, lib, util, ...}:
with lib;
let

  commandModule = import ./command.nix { global = config; inherit util; };

  cli = config.internal.hixCli.exe;

  json = util.json.ghciFile;

in {
  options = with types; {

    commands = mkOption {
      description = "";
      type = attrsOf (submodule commandModule);
      default = {};
    };

  };

  config.commands = {

    ghci = {
      command = ''
      ghci_cmd=$(${cli} ghci-cmd -c ${json} ''${env_args[@]} ''${cmd_args[@]})
      eval $ghci_cmd
      '';
      component = true;
    };

    ghcid = {
      command = ''
      config=$(cat ${util.json.ghciFile})
      ghcid_cmd=$(${cli} ghcid-cmd -c ${json} ''${env_args[@]} ''${cmd_args[@]})
      eval $ghcid_cmd
      '';
      component = true;
    };

  };
}
