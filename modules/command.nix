{global, ...}:
{name, config, lib, ...}:
with lib;
let

  envModule = import ./env.nix { inherit global; };

in {
  options = with types; {

    name = mkOption {
      description = "Name";
      type = str;
      default = name;
    };

    env = mkOption {
      description = "The env for the command";
      type = submodule envModule;
      default = {};
    };

    command = mkOption {
      type = str;
    };

    path = mkOption {
      description = "The final executable.";
      type = path;
    };

  };

  config = {

    path = global.pkgs.writeScript "hix-command-${config.name}" ''
    #!${global.pkgs.bashInteractive}/bin/bash
    ${config.env.code}
    ${config.command}
    '';

  };
}
