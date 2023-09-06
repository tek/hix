{config, util}:
{command, env}:
let

  inherit (config.internal) pkgs;

  cli = config.internal.hixCli.exe;

  splitArgs = ''
  declare -a env_args=() cmd_args=()
  found=false
  for a in $@
  do
    if [[ $found == true ]]
    then
      cmd_args+=($a)
    elif [[ $a == '--' ]]
    then
      found=true
    else
      env_args+=($a)
    fi
  done
  '';

  exe = pkgs.writeScript "command-${command.name}" command.command;

  json = util.json.envFile env;

  script =
    if command.component
    then ''
    ${splitArgs}
    env_runner=$(${cli} env --config ${json} "''${env_args[@]}")
    env_args="''${env_args[*]}" $env_runner "${exe} "''${cmd_args[@]}""
    ''
    else ''
    ${env.code}
    ${exe} "$@"
    '';

in {

  inherit script;

  path = pkgs.writeScript "hix-command-${command.name}" ''
  #!${pkgs.bashInteractive}/bin/bash
  set -eu
  ${script}
  '';

}
