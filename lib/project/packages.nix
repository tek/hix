{util}: let

  inherit (util) config lib;

  package = name: conf: {

    path =
      if conf.relativePath == null
      then util.path.relative conf.src
      else conf.relativePath
      ;

  };

in lib.mapAttrs package config.packages
