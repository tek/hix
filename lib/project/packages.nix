{util}: let

  inherit (util) config lib internal;

  managedVersion = name: let
    state = internal.managed.state.current;
  in state.packages.${name}.version or "0.1.0.0";

  package = name: conf: {

    path =
      if conf.relativePath == null
      then internal.path.relative conf.src
      else conf.relativePath
      ;

    version =
      if conf.cabal-config.version == null
      then managedVersion name
      else conf.cabal-config.version
      ;

  };

in lib.mapAttrs package config.packages
