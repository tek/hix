{util}:
let

  inherit (util) lib;

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

  cli = util.config.internal.hixCli.exe;

  ghciJson = util.json.ghciFile;

  ghciCommand = lib.makeExtensible (self: {

    config = {};

    cliCmd = if self.config.ghcid or false then "ghcid-cmd" else "ghci-cmd";

    options = let
      opt = switch: o: lib.optionalString (self.config.${o} != null) " ${switch} ${self.config.${o}}";
    in "${opt "-r" "runner"}${opt "-p" "package"}${opt "-m" "module"}${opt "-c" "component"}";

    script = ''
    ghci_cmd=$(${cli} ${self.cliCmd} --config ${ghciJson} ${self.options} ''${env_args[@]} "''$@")
    eval $ghci_cmd
    '';

  });

  inEnv = {command, env}:
  lib.makeExtensible (self: {

    exe = util.scriptErr "command-${command.name}" command.command;

    inherit cli;

    json = util.json.envFile env;

    script =
      if command.component
      then ''
      ${splitArgs}
      env_runner=$(${self.cli} env --config ${self.json} "''${env_args[@]}")
      env_args="''${env_args[*]}" $env_runner "${self.exe} "''${cmd_args[@]}""
      ''
      else ''
      ${env.code}
      ${self.exe} "$@"
      '';

    path = util.script "hix-command-${env.name}-${command.name}" ''
    set -u
    ${self.script}
    '';

  });

in {
  inherit
  ghciCommand
  inEnv
  ;
}
