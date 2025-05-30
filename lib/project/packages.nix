{util}: let

  inherit (util) config lib internal;

  package = name: conf: {

    path =
      if conf.relativePath == null
      then internal.path.relative conf.src
      else conf.relativePath
      ;

  };

in lib.mapAttrs package config.packages
