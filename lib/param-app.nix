{ config, lib, util, ... }:
{
  name,
  func,
  params,
  pre ? "",
  shellName ? name,
}:
let

  inherit (config) pkgs;

  param = i: p: ''$(arg "${p}" ${toString i})'';

  paramLines = lib.concatStringsSep "\n  " (lib.imap1 param params);

  script = cmd: pkgs.writeScriptBin "hix-param-app" (toString cmd);

  shell = args: let
    cmd = func args;
    command = if lib.isDerivation cmd then cmd else cmd.command;
    shell = if lib.isDerivation cmd then pkgs.mkShell {} else cmd.shell;
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
      if [[ $num_args > ${toString (lib.length params)} ]]
      then
        print "Usage: nix run .#${name} -- ${lib.concatStringsSep " " params}"
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
      nix --quiet --quiet --quiet --show-trace develop --impure --expr "(builtins.getFlake path:$PWD).legacyPackages.${config.system}.__hix-internal__.project.${shellName} $funargs" -c hix-param-app
  '';

  app = {
    type = "app";
    program = toString appScript;
  };

in {
  inherit shell appScript app;
}
