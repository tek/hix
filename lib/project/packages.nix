{util}: let

  inherit (util) config lib internal project;

  managedVersion = name:
    internal.managed.state.current.packages.${name}.version or null;

  versionFromFile = f:
    if f != null && lib.hasSuffix ".nix" f
    then import "${project.base}/${f}"
    else null;

  package = name: conf: {

    path =
      if conf.relativePath == null
      then internal.path.relative conf.src
      else conf.relativePath
      ;

    version =
      lib.findFirst (a: a != null) "0.1.0.0" [
        conf.cabal-config.version
        (managedVersion name)
        (versionFromFile conf.versionFile)
        (versionFromFile config.release.versionFile)
      ];

  };

in lib.mapAttrs package config.packages
