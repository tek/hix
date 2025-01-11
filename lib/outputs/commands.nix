{util}: let

  inherit (util) build lib;

  paths = util.mapValues (cmd: cmd.path);

  apps = cmds: util.mapValues util.app (paths cmds);

in {

  legacyApps.cmd = apps build.commands.default;

  apps = apps (lib.filterAttrs (_: c: c.expose) build.commands.default);

  appsFull.env = util.mapValues apps build.commands.envs;

}
