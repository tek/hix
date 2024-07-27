{util}: let

  inherit (util) config internal;

in {

  legacyApps = {
    hpack = internal.warn.deprecatedApp "hpack" ".#gen-cabal" config.hpack.script;
    hpack-quiet = internal.warn.deprecatedApp "hpack-quiet" ".#gen-cabal-quiet" config.hpack.scriptQuiet;
  };

}
