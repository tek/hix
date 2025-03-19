{util}: {env}: let

  inherit (util) config lib internal build;

  built = build.envs.${env.name};

  exportShellVars = vars: let
    defs = lib.toShellVars env.env;
    exports = util.unlinesMap (n: "export ${n}") (lib.attrNames env.env);
  in lib.optionalString (!(util.empty vars)) ''
  ${defs}
  ${exports}
  '';

  messages = util.unlinesConcatMap (s: map (m: ''echo ">>> ${m}" >&2'') (s.messages env)) built.resolvedServices;

in ''
_hix_unrestricted() {
  [[ -z ''${DIRENV_IN_ENVRC-} ]] && [[ -z ''${HIX_ONLY_ENV-} ]]
}
quitting=0
quit() {
  if [[ $quitting == 0 ]]
  then
    quitting=1
    if [[ -n ''${1-} ]]
    then
      echo ">>> Terminated by signal $1" >&2
    fi
    ${env.exit-pre}
    ${lib.optionalString env.vm.enable env.vm.exit}
    ${env.exit}
    # kill zombie GHCs
    ${config.pkgs.procps}/bin/pkill -9 -x -P 1 ghc || true
  fi
  if [[ -n ''${1-} ]]
  then
    exit 1
  fi
}
if _hix_unrestricted
then
  if [[ -z ''${_hix_is_shell-} ]]
  then
    trap "quit INT" INT
  fi
  trap "quit TERM" TERM
  trap "quit KILL" KILL
  trap quit EXIT
fi
${exportShellVars env.env}
export PATH="${lib.makeBinPath built.buildInputs}:$PATH"
export env_args
if _hix_unrestricted
then
  :
  ${messages}
  ${env.setup-pre}
  ${lib.optionalString env.vm.enable env.vm.setup}
  ${lib.optionalString (env.vm.enable && env.wait > 0) (internal.env.scripts.wait env)}
  ${env.setup}
fi
''
