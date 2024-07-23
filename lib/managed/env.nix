{util}: let
  inherit (util) config lib;
  conf = config.managed;

  envDefault = lib.mkOverride 500;

  envFor = packages: special: {
    inherit packages;
    managed = lib.mkDefault true;
    expose = lib.mkDefault true;
  } // special.envs.verbatim // {
    ghc = { compiler = envDefault special.compiler; } // special.envs.verbatim.ghc or {};
    internal = { overridesSolver = envDefault special.envs.solverOverrides; } // special.envs.verbatim.internal or {};
  };

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
