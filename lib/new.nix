{pkgs}: let

  prog = config.pkgs.writeScript "hix-new" ''
  #!${config.pkgs.bashInteractive}/bin/bash
  set -e
  ${config.internal.hixCli.package}/bin/hix new $@
  ${pkgs.git}/bin/git init
  ${pkgs.git}/bin/git add .
  '';

in {
  inherit prog;
}
