{util}: let

  inherit (util) config internal;

in {

  deprecatedApps = {
    hpack = internal.warn.deprecatedLegacyApp "hpack" ".#gen-cabal" config.hpack.script;
    hpack-quiet = internal.warn.deprecatedLegacyApp "hpack-quiet" ".#gen-cabal-quiet" config.hpack.scriptQuiet;
  };

}
