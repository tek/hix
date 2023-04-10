{pkgs}: let

  prog = config.pkgs.writeScript "hix-new" ''
  #!${config.pkgs.bashInteractive}/bin/bash
  set -e
  ${config.internal.hixCli.package}/bin/hix new $@
  if [[ ! -e .git ]]
  then
    ${pkgs.git}/bin/git init
  fi
  ${pkgs.git}/bin/git add .
  '';

in {
  inherit prog;
}
