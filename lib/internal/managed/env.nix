{util}: let
  inherit (util) config lib internal;
  conf = config.managed;

  staticConfig = {
    package-set.compiler = {
      nixpkgs.config.config.allowBroken = true;
    };
    managed = lib.mkDefault true;
    # TODO use fine-grained variant
    expose = lib.mkDefault true;
  };

  derivedConfig = conf: {
    package-set.compiler.source = internal.modules.envDefault conf.compiler;
    internal.overridesSolver = internal.modules.envDefault conf.envs.solverOverrides;
  };

  envFor = managedBound: packages: special:
  lib.mkMerge [
    { inherit packages managedBound; }
    staticConfig
    (derivedConfig special)
    special.envs.verbatim
  ];

  envsFor = suf: packages: {
    ${"latest${suf}"} = envFor "upper" packages config.managed.latest;
  } // lib.optionalAttrs conf.lower.enable {
    ${"lower${suf}"} = envFor "lower" packages config.managed.lower;
  };

  envsAll = envsFor "" null;

  envsSingle = name: envsFor "-${name}" [name];

  envsSet = name: envsFor "-${name}";

  envsEach = util.mapListCatAttrs envsSingle internal.project.packageNames;

  envsSets = lib.concatMapAttrs envsSet conf.sets;

  modules = let

    value =
      if conf.sets == "all"
      then envsAll
      else if conf.sets == "each"
      then envsEach
      else envsSets
      ;

  in lib.optionalAttrs conf.enable value;

  envs = lib.mapAttrs (name: _: config.envs.${name}) modules;

in {
  inherit modules envs;
}
