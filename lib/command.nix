{config, util}:
{command, env}:
let

  inherit (config) pkgs;

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

  script =
    if command.component
    then ''
    config=$(cat ${util.json.packagesFile})
    ${splitArgs}
    env_runner=$(${cli} component-env -c "$config" ''${env_args[@]})
    env_run()
    {
      $env_runner $@
    }
    ${command.command}
    ''
    else ''
    ${env.code}
    ${command.command}
    '';

in {

  inherit script;

  path = pkgs.writeScript "hix-command-${command.name}" ''
  #!${pkgs.bashInteractive}/bin/bash
  set -eu
  ${script}
  '';

}
