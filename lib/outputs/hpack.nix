{util}: let

  inherit (util) config;

in {

  legacyApps = {
    hpack = config.hpack.script;
    hpack-quiet = config.hpack.scriptQuiet;
  };

}
