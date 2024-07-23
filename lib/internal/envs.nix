{util}: let

  inherit (util) internal config lib;

  # mapMaybe ::
  #   (Maybe Env -> a -> Maybe b) ->
  #   Map EnvName a ->
  #   Map EnvName b
  mapMaybe = f: let
    mapEnv = envName: a: f (config.envs.${envName} or null) a;
  in util.mapMaybe mapEnv;

  # bimap ::
  #   (Maybe Env -> Map PackageName b -> Maybe c) ->
  #   (Maybe Env -> Package -> a -> Maybe b) ->
  #   Map EnvName (Map PackageName a) ->
  #   Map EnvName c
  bimap = fe: fp:
  mapMaybe (env: packages: fe env (internal.packages.mapMaybe (fp env) packages));

  map = bimap (_: a: a);

  extractTargets = env: let
    ts = internal.env.targets env ++ config.output.extraPackages;
  in lib.filterAttrs (pkgName: _: lib.elem pkgName ts);

  bimapTargets = fe: fp: let
    ft = envName: packages: let
      env = config.envs.${envName};
    in fe env (internal.packages.mapMaybe (fp env) (extractTargets env packages));
  in util.mapMaybe ft;

  mapTargets = bimapTargets (_: a: a);

  filterExposedPackages = purpose:
  map (_: internal.package.justExposed purpose);

  filterExposed = purpose:
  mapMaybe (internal.env.justExposed purpose);

in {
  inherit
  mapMaybe
  bimap
  map
  extractTargets
  bimapTargets
  mapTargets
  filterExposedPackages
  filterExposed
  ;
}
