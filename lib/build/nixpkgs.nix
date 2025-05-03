{util}: let

  inherit (util) lib;
  pkgs = util.vanillaPkgs;

  isPkgs = x: (x._type or null) == "pkgs";

  reimport = old: args:
  old.nixpkgsFun {
    overlays = (old.overlays or []) ++ (args.overlays or []);
    # TODO `old.config` can be a function `{pkgs}: {...}`
    config = (old.config or {}) // (args.config or {});
  };

  setup = conf:
    if conf.rev == null
    then { fetch = pkgs.fetchurl; args = {}; }
    else { fetch = pkgs.fetchgit; args = { inherit (conf) rev; }; }
    ;

  resolveSource = conf:
  if isPkgs conf
  then reimport conf
  else if builtins.isPath conf || lib.isDerivation conf || lib.isStorePath conf
  then import conf
  else import setup.fetch ({ inherit (conf) url hash; } // setup.args // conf.args);

  compileArgs = conf: overlays: { config = conf.config; inherit overlays; } // conf.args;

  create = conf: lib.makeExtensible (final: {

    construct = resolveSource conf.source;

    overlays = conf.overlays;

    args = compileArgs conf final.overlays;

    pkgs = final.construct final.args;

  });

in util.mapValues create util.config.nixpkgs // { __functor = _: create; }
