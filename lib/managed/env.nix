{util}: let
  inherit (util) config lib;
  conf = config.managed;

  envDefault = lib.mkOverride 500;

  modules = let

    envFor = packages: special: {
      inherit packages;
      ghc.compiler = envDefault special.compiler;
      internal.overridesSolver = envDefault special.envs.solverOverrides;
    } // special.envs.verbatim;

    envsFor = suf: packages: {
      ${"latest${suf}"} = envFor packages config.managed.latest;
    } // lib.optionalAttrs conf.lower.enable {
      ${"lower${suf}"} = envFor packages config.managed.lower;
    };

    envsAll = envsFor "" null;

    envsSingle = name: envsFor "-${name}" [name];

    envsSet = name: envsFor "-${name}";

    envsEach = util.mapListCatAttrs envsSingle config.internal.packageNames;

    envsSets = lib.concatMapAttrs envsSet conf.sets;

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
