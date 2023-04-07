{config, lib, util, ...}:
with lib;
let

  commandModule = import ./command.nix { global = config; inherit util; };

  cli = config.internal.hixCli.exe;

  json = util.json.ghciFile;

in {
  options = with types; {

    commands = mkOption {
      description = mdDoc ''
      Commands are shell scripts associated with an environment that are exposed as flake apps.
      All commands are accessible as attributes of `.#cmd.<name>`, and those that set `expose = true` are additionally
      exposed at the top level.
      '';
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
      expose = true;
    };

    ghcid = {
      command = ''
      config=$(cat ${util.json.ghciFile})
      ghcid_cmd=$(${cli} ghcid-cmd -c ${json} ''${env_args[@]} ''${cmd_args[@]})
      eval $ghcid_cmd
      '';
      component = true;
      expose = true;
    };

    hls = {
      env = "dev";
      command = "${config.envs.hls.hls.package}/bin/haskell-language-server $@";
      expose = true;
    };

    run = {
      command = "$@";
    };

  };
}
