{util}: let

  inherit (util) config lib internal;

  buildCommand = env: command:
  (internal.command.inEnv { inherit env command; }).extend (_: _: {
    inherit (command) expose;
  });

  buildCommands = envName: env:
  util.mapValues (buildCommand env) config.commands;

in {

  default = lib.mapAttrs (_: cmd: buildCommand (config.envs.${cmd.env}) cmd) config.commands;

  envs = lib.mapAttrs buildCommands config.envs;

}
