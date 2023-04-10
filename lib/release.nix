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

  sed -i 's/ref=[^"]\+/ref='"$version/" readme.md examples/*/flake.nix
  sed -i 's/hixVersion = ".*"/hixVersion = "'"$version"'"/' modules/basic.nix
  ${config.pkgs.git}/bin/git --no-pager diff
  ${config.pkgs.git}/bin/git add readme.md examples modules/basic.nix

  nix run .#release -- -v $version
  print -n ">>> Update CLI version in overrides. Continue? [Yn] "
  read -q decision
  print ""
  if [[ $decision == 'n' ]]
  then
    print ">>> Aborting."
    exit 1
  fi
  ${config.pkgs.git}/bin/git add modules/cli.nix
  ${config.pkgs.git}/bin/git tag -m "Release $version" "$version"
  '';

in prog
