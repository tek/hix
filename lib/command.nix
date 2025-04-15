{util}:
let

  inherit (util) lib internal outputs;

  cli = util.config.internal.hixCli.exe;

  ghciCommand = env: config: let

    cliCmd = if config.ghcid or false then "ghcid" else "ghci";

    options = let
      opt = switch: o: lib.optionalString (config.${o} != null) " ${switch} ${config.${o}}";
    in "${opt "-r" "runner"}${opt "-p" "package"}${opt "-m" "module"}${opt "-c" "component"}";

    json = outputs.cli-context.json.ghci env;

  in ''
  ${cli} ${cliCmd} --config ${json} ${options} "$@"
  '';

  inEnv = {command, env}:
  lib.makeExtensible (self: {

    exe =
      if lib.isPath command.command
      then command.command
      else util.scriptErr "command-${command.name}" command.command;

    inherit cli;

    json = util.json.envFile env;

    script =
      if command.ghci.enable
      then
      ghciCommand env command.ghci
      else
      if command.component
      then ''
      ${self.cli} command --config ${self.json} --exe "${self.exe}" "$@"
      ''
      else ''
      ${env.code}
      ${self.exe} "$@"
      '';

    path = util.script "hix-command-${env.name}-${command.name}" ''
    set -u
    export PATH="${lib.makeBinPath (internal.env.mkBuildInputs env command.buildInputs)}:$PATH"
    ${self.script}
    '';

  });

in {
  inherit
  ghciCommand
  inEnv
  ;
}
