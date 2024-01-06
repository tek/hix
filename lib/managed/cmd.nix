{util}: let
  inherit (util) config lib;

  cli = config.internal.hixCli.exe;

  conf = config.managed;

  file = conf.file;

  verbose = lib.optionalString conf.verbose "--verbose";

  debug = lib.optionalString conf.debug "--debug";

  quiet = lib.optionalString conf.quiet "--quiet";

  readUpperBounds = lib.optionalString conf.latest.readFlakeBounds "--read-upper-bounds";

  mergeBounds = lib.optionalString conf.mergeBounds "--merge-bounds";

  # TODO change this to use only one script that loops over all the options
  # TODO also generalize
  withCheckFor = flag: name: main:
    if lib.attrByPath flag false conf
    then main
    else config.pkgs.writeScript "managed-disabled" ''
    ${util.loadConsole}
    die "Set $(blue 'managed.${lib.concatStringsSep "." flag} = true;') $(red 'to use this feature.')"
    '';

  mainScript = cmd: envs: let
    general = [
      "--config ${util.managed.state.cliJson}"
      "--file ${file}"
      readUpperBounds
      mergeBounds
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
  lower = sub: checkedScript [["lower" "enable"] ["enable"]] "lower ${sub}";

in {
  inherit bump lower;
}
