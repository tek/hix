{util}: let

  inherit (util) build lib;

  paths = util.mapValues (cmd: cmd.path);

  apps = cmds: util.mapValues util.app (paths cmds);

in {

  apps =
    { cmd = apps build.commands.default; }
    //
    apps (lib.filterAttrs (_: c: c.expose) build.commands.default)
    ;

  appsFull = util.mapValues apps build.commands.envs;

}
