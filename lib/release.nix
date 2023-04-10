{config, lib, util}: let

  prog = config.pkgs.writeScript "hix-release-all" ''
  #!${config.pkgs.bashInteractive}/bin/bash
  set -eu -o pipefail

  if [[ $# == 0 ]]
  then
    echo 'Please specify version'
    exit 1
  fi
  version="$1"

  sed -i 's/tag=v[^"]\+/tag=v'"$version/" readme.md examples/*/flake.nix
  sed -i 's/hixVersion = ".*"/hixVersion = "'"$version"'"/' modules/basic.nix
  ${config.pkgs.git}/bin/git --no-pager diff
  git add readme.md examples modules/basic.nix

  nix run .#release -- -v $version
  '';

in prog
