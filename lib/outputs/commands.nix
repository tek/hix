{util}: let

  inherit (util) build lib;

  paths = util.mapValues (cmd: cmd.path);

  apps = cmds: util.mapValues util.app (paths cmds);

  legacyApps = cmds: util.ensureLegacyApps (paths cmds);

in {

  legacyPackages =
    { cmd = legacyApps build.commands.default; }
    //
    { env = util.mapValues legacyApps build.commands.envs; }
    ;

  apps = apps (lib.filterAttrs (_: c: c.expose) build.commands.default);

}
