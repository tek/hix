{util}: let
  inherit (util) config lib;

  conf = config.managed;

  cli = config.internal.hixCli.exe;

  file = conf.file;

  update = lib.optionalString conf.update "--update-project";

  verbose = lib.optionalString conf.verbose "--verbose";

  debug = lib.optionalString conf.debug "--debug";

  quiet = lib.optionalString conf.quiet "--quiet";

  overrides = env: lib.optionalString env.managedOverrides "--overrides";

  withCheckFor = flag: name: main:
    if lib.attrByPath flag false conf
    then main
    else config.pkgs.writeScript "managed-disabled" ''
    ${util.loadConsole}
    die "Set $(blue 'managed.${lib.concatStringsSep "." flag} = true;') $(red 'to use this feature.')"
    '';

  mainScript = cmd: env: let
    general = [
      "--config ${util.json.managedFile (util.env.targets env)}"
      "--env ${env.name}"
      "--file ${file}"
      "--ghc ${util.ghc.packageDbLocal env}"
      update
      (overrides env)
    ];
    args = util.unwords general;
  in
  config.pkgs.writeScript "managed-${env.name}" ''
  set -e
  ${cli} ${verbose} ${debug} ${quiet} ${cmd} $@ ${args}
  ${lib.optionalString conf.generate (util.runBuildApp "gen${lib.optionalString conf.quiet "-quiet"}")}
  ${lib.optionalString conf.gitAdd ''
    if ${config.pkgs.git}/bin/git status &>/dev/null
    then
      ${config.pkgs.git}/bin/git add ${conf.file}
    fi
  ''}
  '';

  checkedScript = flags: cmd: env: lib.foldl (z: flag: withCheckFor flag env.name z) (mainScript cmd env) flags;

  bump = env: checkedScript [["enable"]] "bump" env;
  lowerInit = env: checkedScript [["lower" "enable"] ["enable"]] "lower init --stabilize" env;
  lowerOptimize = env: checkedScript [["lower" "enable"] ["enable"]] "lower optimize" env;

  envFor = packages: config.managed.envConfig // {
    inherit packages;
  };

  lowerEnvConfigExtra = {
    ghc.compiler = lib.mkOverride 500 config.managed.lower.compiler;
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

  envs =
    lib.optionalAttrs conf.enable (
      if conf.sets == "all"
      then envsAll
      else if conf.sets == "each"
      then envsEach
      else envsSets
    );

in {
  inherit bump lowerInit lowerOptimize envs;
}
