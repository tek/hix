{util}:
let

  inherit (util) lib outputs;

  cli = util.config.internal.hixCli.exe;

  ghciCommand = env: config: let

    cliCmd = if config.ghcid or false then "ghcid" else "ghci";

    extra =
      if (config.ghcid or false) && util.config.ghcid.args != []
      then " --ghcid-args='${util.unwords util.config.ghcid.args}'"
      else "";

    options = let
      opt = switch: o: lib.optionalString (config.${o} != null) " ${switch} ${config.${o}}";
    in "${opt "-r" "runner"}${opt "-p" "package"}${opt "-m" "module"}${opt "-c" "component"}";

    json = outputs.cli-context.json.ghci.${env.name};

  in ''
  ${cli} ${cliCmd} --context ${json} ${options}${extra} "$@"
  '';

  inEnv = {command, env}:
  lib.makeExtensible (self: {

    exe =
      if lib.isPath command.command
      then command.command
      else util.scriptErr "command-${command.name}" command.command;

    inherit cli;

    json = outputs.cli-context.json.ghci.${env.name};

    script =
      if command.ghci.enable
      then
      ghciCommand env command.ghci
      else
      if command.component
      then ''
      ${self.cli} command --context ${self.json} --exe "${self.exe}" "$@"
      ''
      else ''
      ${env.code}
      ${self.exe} "$@"
      '';

    path = util.script "hix-command-${env.name}-${command.name}" ''
    set -u
    export PATH="${lib.makeBinPath (command.buildInputs env.toolchain.pkgs)}:$PATH"
    ${self.script}
    '';

  });

in {
  inherit
  ghciCommand
  inEnv
  ;
}
