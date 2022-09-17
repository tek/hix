{ config, lib, ... }:
{
  name,
  func,
  pre ? "",
}:
with lib;
let

  inherit (config) pkgs;

  params = attrNames (functionArgs func);

  dollar = "$";

  param = i: p: ''$(arg "${p}" ${toString i})'';

  paramLines = concatStringsSep "\n" (imap1 param params);

  script = args: pkgs.writeScriptBin "hix-param-app" ''
  ${func args}
  '';

  shell = args: pkgs.mkShell {
    packages = [(script args)];
  };

  app =
    pkgs.writeScript "${name}-app" ''
      #!${pkgs.zsh}/bin/zsh
      num_args=$#
      args=($@)
      arg()
      {
        if (( $2 >= $num_args ))
        then
          print "$1 = \"$args[$2]\";"
        else
          exit 1
        fi
      }
      ${pre}
      nix develop --impure --expr "
        (builtins.getFlake path:$PWD).legacyPackages.${config.system}.${name} {
          ${paramLines}
        }" -c hix-param-app
  '';

in {
  inherit shell app;
}
