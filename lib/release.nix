{config, lib, util}: let

  tag = ''
  ${config.pkgs.git}/bin/git tag -m "Release $version" "$version"
  '';

  updateVersions = ''
  sed -i 's/ref=[^"]\+/ref='"$version/" readme.md examples/*/flake.nix
  sed -i 's/hixVersion = ".*"/hixVersion = "'"$version"'"/' modules/basic.nix
  ${config.pkgs.git}/bin/git --no-pager diff
  ${config.pkgs.git}/bin/git add readme.md examples modules/basic.nix
  '';

  preamble = ''
  #!${config.pkgs.bashInteractive}/bin/bash
  set -eu -o pipefail

  if [[ $# == 0 ]]
  then
    echo 'Please specify version'
    exit 1
  fi
  version="$1"
  '';

  nix = config.pkgs.writeScript "hix-release-nix" ''
  ${preamble}
  ${updateVersions}
  ${tag}
  '';

  all = config.pkgs.writeScript "hix-release-all" ''
  ${preamble}
  ${updateVersions}

  nix run .#release -- -v $version
  echo -n ">>> Update CLI version in overrides. Continue? [Yn] "
  read -q decision
  echo ""
  if [[ $decision == 'n' ]]
  then
    echo ">>> Aborting."
    exit 1
  fi
  ${config.pkgs.git}/bin/git add modules/cli.nix
  ${tag}
  '';

in { inherit nix all; }
