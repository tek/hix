{config, lib, ...}:
with lib;
let

  commandModule = import ./command.nix { global = config; };

  ghcidCommand = ''
  config=$(<${toJSON ghcidConfig})
  '';

in {
  options = with types; {

    commands = mkOption {
      description = "";
      type = attrsOf (submodule commandModule);
      default = {};
    };

  };

  config = {

    commands.ghcid = {
      env = mkDefault config.defaultEnv;
      command = ghcidCommand;
    };

  };
}
