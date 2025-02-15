{util}: let

  inherit (util) internal config lib just;

  noEnv = name: ''
  An expression using the tools in `internal.envs` to transform values in `build` has encountered a key
  that does not correspond to an existing environment in `config.envs`.
  '';

  mapEnv = f: envName: let

    env = config.envs.${envName} or null;

  in internal.warn.warnEval (env == null) "internal.envs.map.unknown-env" (noEnv envName) (f env);

  # map ::
  #   (Env -> a -> b) ->
  #   Map EnvName a ->
  #   Map EnvName b
  map = f: lib.mapAttrs (mapEnv f);

  # bimap ::
  #   (Env -> Map PackageName b -> Maybe c) ->
  #   (Env -> Maybe Package -> a -> Maybe b) ->
  #   Map EnvName (Map PackageName a) ->
  #   Map EnvName c
  bimap = fe: fp:
  map (env: packages: fe env (internal.packages.map (fp env) packages));

  # mapMaybe ::
  #   (Env -> a -> Maybe b) ->
  #   Map EnvName a ->
  #   Map EnvName b
  mapMaybe = f: util.mapMaybe (mapEnv f);

  # bimapMaybe ::
  #   (Env -> Map PackageName b -> Maybe c) ->
  #   (Env -> Package -> a -> Maybe b) ->
  #   Map EnvName (Map PackageName a) ->
  #   Map EnvName c
  bimapMaybe = fe: fp:
  mapMaybe (env: packages: fe env (internal.packages.mapMaybe (fp env) packages));

  # mapMaybePkgs ::
  #   (Env -> Package -> a -> Maybe b) ->
  #   Map EnvName (Map PackageName a) ->
  #   Map EnvName (Map PackageName b)
  mapMaybePkgs = bimapMaybe (_: just);

  # extractTargets ::
  #   Env ->
  #   Map PackageName a ->
  #   Map PackageName b
  extractTargets = env: let
    ts = internal.env.targets env ++ config.output.extraPackages;
  in lib.filterAttrs (pkgName: _: lib.elem pkgName ts);

  # bimapTargets ::
  #   (Env -> Map PackageName b -> c) ->
  #   (Env -> Package -> a -> b) ->
  #   Map EnvName (Map PackageName a) ->
  #   Map EnvName c
  bimapTargets = fe: fp:
  map (env: packages: fe env (internal.packages.map (fp env) (extractTargets env packages)));

  # mapTargets ::
  #   (Env -> Package -> a -> b) ->
  #   Map EnvName (Map PackageName a) ->
  #   Map EnvName (Map PackageName b)
  mapTargets = bimapTargets (_: a: a);

  # bimapMaybeTargets ::
  #   (Env -> Map PackageName b -> Maybe c) ->
  #   (Env -> Package -> a -> Maybe b) ->
  #   Map EnvName (Map PackageName a) ->
  #   Map EnvName c
  bimapMaybeTargets = fe: fp:
  mapMaybe (env: packages: fe env (internal.packages.mapMaybe (fp env) (extractTargets env packages)));

  # mapMaybeTargets ::
  #   (Env -> Package -> a -> Maybe b) ->
  #   Map EnvName (Map PackageName a) ->
  #   Map EnvName (Map PackageName b)
  mapMaybeTargets = bimapMaybeTargets (_: just);

  # filterEnabled ::
  #   Map EnvName a ->
  #   Map EnvName a
  filterEnabled =
  mapMaybe internal.env.justEnabled;

  # filterExposedPackages ::
  #   Map EnvName (Map PackageName a) ->
  #   Map EnvName (Map PackageName a)
  filterExposedPackages = purpose:
  mapMaybePkgs (_: internal.package.justExposed purpose);

  # filterExposed ::
  #   Map EnvName a ->
  #   Map EnvName a
  filterExposed = purpose:
  mapMaybe (internal.env.justExposed purpose);

in {
  inherit
  bimap
  map
  mapMaybe
  bimapMaybe
  mapMaybePkgs
  extractTargets
  bimapTargets
  mapTargets
  bimapMaybeTargets
  mapMaybeTargets
  filterEnabled
  filterExposedPackages
  filterExposed
  ;
}
