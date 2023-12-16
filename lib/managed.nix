{util}: let
  inherit (util) config lib;

  conf = config.managed;

  cli = config.internal.hixCli.exe;

  file = conf.file;

  update = lib.optionalString conf.update "--update-project";

  verbose = lib.optionalString conf.verbose "--verbose";

  debug = lib.optionalString conf.debug "--debug";

  quiet = lib.optionalString conf.quiet "--quiet";

  # TODO change this to use only one script that loops over all the options
  withCheckFor = flag: name: main:
    if lib.attrByPath flag false conf
    then main
    else config.pkgs.writeScript "managed-disabled" ''
    ${util.loadConsole}
    die "Set $(blue 'managed.${lib.concatStringsSep "." flag} = true;') $(red 'to use this feature.')"
    '';

  mainScript = cmd: envs: let
    general = [
      "--config ${util.json.managedFile}"
      "--file ${file}"
      update
    ];

    envsArgs = map (env: "--env ${env}") envs;

    args = util.unwords (general ++ envsArgs);

    desc = if lib.length envs == 1 then lib.head envs else "${cmd}-multi";
  in
  config.pkgs.writeScript "managed-${desc}" ''
  #!${config.pkgs.zsh}/bin/zsh
  setopt err_exit
  if [[ -e "${conf.file}" ]]
  then
    initial=false
  else
    initial=true
  fi
  ${cli} ${verbose} ${debug} ${quiet} ${cmd} $@ ${args}
  ${lib.optionalString conf.generate (util.runBuildApp "gen${lib.optionalString conf.quiet "-quiet"}")}
  ${lib.optionalString conf.gitAdd ''
    if ${config.pkgs.git}/bin/git status &>/dev/null && [[ $initial == true ]] && [[ -f ${conf.file} ]]
    then
      ${config.pkgs.git}/bin/git add ${conf.file}
    fi
  ''}
  '';

  checkedScript = flags: cmd: envs:
  lib.foldl (z: flag: lib.foldl (z': env: withCheckFor flag env z') z envs) (mainScript cmd envs) flags;

  bump = checkedScript [["enable"]] "bump";
  lowerInit = checkedScript [["lower" "enable"] ["enable"]] "lower init";
  lowerOptimize = checkedScript [["lower" "enable"] ["enable"]] "lower optimize";
  lowerStabilize = checkedScript [["lower" "enable"] ["enable"]] "lower stabilize";

  envModules = let

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

  envs = lib.mapAttrs (name: _: config.envs.${name}) envModules;

  envState = let
    file = "${config.base}/${config.managed.file}";
  in
  { bounds = {}; overrides = {}; resolving = false; } //
  lib.optionalAttrs (config.managed.enable && lib.pathExists file) (import file);

in {
  inherit bump lowerInit lowerOptimize lowerStabilize envModules envs envState;
}
