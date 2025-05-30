{util}: let
  inherit (util) config lib outputs;

  cli = config.internal.hixCli.exe;

  conf = config.managed;

  file = conf.file;

  verbose = lib.optionalString conf.verbose "--verbose";

  debug = lib.optionalString conf.debug "--debug";

  quiet = lib.optionalString conf.quiet "--quiet";

  readUpperBounds = lib.optionalString conf.latest.readFlakeBounds "--read-upper-bounds";

  mergeBounds = lib.optionalString conf.mergeBounds "--merge-bounds";

  sharedOptions = [
    "--file ${file}"
  ];

  withCheckFor = main: flag:
    if lib.attrByPath flag false config
    then main
    else util.zscriptErrBin "managed-disabled" ''
    ${util.loadConsole}
    die "Set $(blue '${lib.concatStringsSep "." flag} = true;') $(red 'to use this feature.')"
    '';

  cliJson = outputs.cli-context.json.managed;

  managedScript = cmd: envs: let
    general = [
      "--context ${cliJson}"
      readUpperBounds
      mergeBounds
    ];

    envsArgs = map (env: "--env ${env}") envs;

    args = util.unwords (sharedOptions ++ general ++ envsArgs);

    desc = if lib.length envs == 1 then lib.head envs else "${cmd}-multi";
  in
  util.zscriptBin "managed-${desc}" ''
  if [[ -e "${conf.file}" ]]
  then
    initial=false
  else
    initial=true
  fi
  ${cli} ${verbose} ${debug} ${quiet} ${cmd} $@ ${args}
  ${lib.optionalString conf.generate (util.runBuildApp "gen${lib.optionalString (!util.managed.minVerbose) "-quiet"} --quiet")}
  ${lib.optionalString conf.gitAdd ''
    if ${config.pkgs.git}/bin/git status &>/dev/null && [[ $initial == true ]] && [[ -f ${conf.file} ]]
    then
      ${config.pkgs.git}/bin/git add ${conf.file}
    fi
  ''}
  '';

  maintScript = util.zscriptBin "managed-maint" ''
  ${cli} ${verbose} ${debug} ${quiet} hackage maint ${util.unwords sharedOptions} $@
  '';

  revisionScript = util.zscriptBin "managed-revision" ''
  ${cli} ${verbose} ${debug} ${quiet} hackage revision $@
  '';

  checkedScript = flags: script:
  lib.foldl withCheckFor script flags;

  checkedMainScript = flags: cmd: envs:
  checkedScript flags (managedScript cmd envs);

  bump = checkedMainScript [["managed" "enable"]] "bump";
  lower = sub: checkedMainScript [["managed" "lower" "enable"] ["managed" "enable"]] "lower ${sub}";
  maint = checkedScript [["ui" "experimental" "managed-maint"] ["managed" "enable"]] maintScript;
  revision = checkedScript [["ui" "experimental" "managed-maint"] ["managed" "enable"]] revisionScript;

in {
  inherit bump lower maint revision;
}
