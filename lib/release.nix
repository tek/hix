{config, lib, util}: let

  prog = config.pkgs.writeScript "hix-release-all" ''
  #!${pkgs.bashInteractive}/bin/bash
  set -eu -o pipefail

  if [[ $# == 0 ]]
  then
    echo 'Please specify version'
    exit 1
  fi
  version="$1"
  '';

in prog
