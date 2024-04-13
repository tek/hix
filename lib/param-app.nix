{ config, lib, ... }:
{
  name,
  func,
  params,
  pre ? "",
  shellName ? name,
}:
with lib;
let

  inherit (config) pkgs;

  param = i: p: ''$(arg "${p}" ${toString i})'';

  paramLines = concatStringsSep "\n  " (imap1 param params);

  script = cmd: pkgs.writeScriptBin "hix-param-app" (toString cmd);

  shell = args: let
    cmd = func args;
    command = if isDerivation cmd then cmd else cmd.command;
    shell = if isDerivation cmd then pkgs.mkShell {} else cmd.shell;
  in shell.overrideAttrs (old: {
    nativeBuildInputs = old.nativeBuildInputs or [] ++ [(script command)];
  });

  appScript =
    util.zscriptErr "${name}-app" ''
      num_args=$#
      argstr="$@"
      args=($@)
      arg()
      {
        if (( $2 <= $num_args ))
        then
          print "$1 = \"$args[$2]\";"
        fi
      }
      if [[ $num_args > ${toString (length params)} ]]
      then
        print "Usage: nix run .#${name} -- ${concatStringsSep " " params}"
        exit 1
      fi
      if [[ $argstr[1] == '{' ]]
      then
        funargs=$argstr
      else
        funargs="{
        ${paramLines}
      }"
      fi
      ${pre}
      nix develop --impure --expr "(builtins.getFlake path:$PWD).legacyPackages.${config.system}.${shellName} $funargs" -c hix-param-app
  '';

  app = {
    type = "app";
    program = toString appScript;
  };

in {
  inherit shell appScript app;
}
