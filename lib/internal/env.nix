{util}: let

  inherit (util) config internal lib just justIf justAttr bindMaybe colors;

  scripts = {

    wait = import ./scripts/wait.nix { inherit util; };

    setup = import ./scripts/setup.nix { inherit util; };

  };

  withEnv = f: envName: f config.envs.${envName};

  envMainPackage = env: let
    targets = internal.env.targets env;
  in
    if lib.elem config.main targets
    then just config.main
    else internal.packages.selectMain targets;

  withMain = alt: f: env:
  util.maybe alt f (bindMaybe (pkg: justAttr pkg config.packages) (envMainPackage env));

  setWithMain = withMain {};

  withExe = alt: f:
  withMain alt (main: internal.package.withExe alt (f main) main);

  setWithExe = withExe {};

  prefixedWith = envName: util.mapKeys (n: _: "${envName}-${n}");

  prefixed = lib.concatMapAttrs prefixedWith;

  # TODO do we need `systemAllowed` here?
  justEnabled = env:
  justIf (env.enable or false);

  isExposed = purpose: item:
  if isNull item
  then false
  else
  if lib.isAttrs item.expose
  then item.expose.${purpose}
  else item.expose;

  justExposed = purpose: env:
  justIf (isExposed purpose env);

  targets = env:
  if (env.packages or null) == null
  then lib.attrNames (lib.filterAttrs (_: isExposed "target") config.packages)
  else env.packages;

  isTarget = envName: pkgName:
  lib.elem pkgName (withEnv targets envName);

  unknownHackage = package: name: ''
  The managed dependency override for ${colors.package package} refers to the nonexistent Hackage server config ${colors.option name}.
  If you recently removed the option definition ${colors.option "hackage.repos.${name}"}, please re-run ${colors.shell_cmd "bump"} and/or ${colors.shell_cmd "lower"}, possibly deleting ${colors.path config.managed.file} beforehand.
  '';

  invalidOverride = package: missing: throw ''
  Internal error: A managed override for '${package}' is missing the attribute '${missing}'.
  '';

  managedOverride = api: package: {version ? null, hash ? null, repo ? null, jailbreak ? null, local ? null, ...}: let
    hackage = if repo == null then api.hackage else api.hackageConfGen (unknownHackage package) repo;
  in
  if version != null && hash != null
  then api.jailbreak (api.notest (api.nodoc (hackage version hash)))
  else if version != null
  then invalidOverride package "hash"
  else if hash != null
  then invalidOverride package "version"
  else if jailbreak == true
  then api.jailbreak
  else if local == true
  then api.source.root config.packages.${package}.src
  else throw "Internal error: Managed override for '${package}' has no attributes."
  ;

  managedOverrides = envName: api: let
    os = util.managed.state.current.overrides.${envName} or {};
  in lib.mapAttrs (managedOverride api) os;

  managedSolverOverrides = envName: api: let
    os = util.managed.state.current.solver.${envName} or {};
  in lib.mapAttrs (managedOverride api) os;

  mkBuildInputs = env: spec:
  if lib.isFunction spec
  then spec env.ghc.pkgs
  else spec;

  buildInputs = env:
  mkBuildInputs env env.buildInputs ++
  mkBuildInputs env config.buildInputs ++
  env.haskellTools env.ghc.vanillaGhc ++
  config.haskellTools env.ghc.vanillaGhc ++
  lib.optional env.hls.enable env.hls.package ++
  lib.optional env.ghcid.enable env.ghcid.package ++
  [env.ghcWithPackages]
  ;

in {
  inherit
  scripts
  withEnv
  withMain
  setWithMain
  withExe
  setWithExe
  prefixedWith
  prefixed
  justEnabled
  isExposed
  justExposed
  targets
  isTarget
  managedOverrides
  managedSolverOverrides
  mkBuildInputs
  buildInputs
  ;
}
