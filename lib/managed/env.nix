{util}: let
  inherit (util) config lib;
  conf = config.managed;

  modules = let

    lowerEnvConfigExtra = {
      ghc.compiler = lib.mkOverride 500 config.managed.lower.compiler;
    };

    envFor = packages: config.managed.envConfig // {
      inherit packages;
    };

    envsFor = suf: packages: {
      ${"latest${suf}"} = envFor packages;
    } // lib.optionalAttrs conf.lower.enable {
      ${"lower${suf}"} = envFor packages // lowerEnvConfigExtra;
    };

    envsAll = envsFor "" null;

    envsSingle = name: envsFor "-${name}" [name];

    envsSet = name: envsFor "-${name}";

    envsEach = util.foldMapAttrs envsSingle config.internal.packageNames;

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
