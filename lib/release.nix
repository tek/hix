{config, lib, util}: let

  commitAndTag = ''
  ${config.pkgs.git}/bin/git commit --allow-empty -m "Release $version"
  ${config.pkgs.git}/bin/git tag -m "Release $version" "$version"
  '';

  updateVersions = ''
  nix run .#test
  sed -i 's/ref=[^"]\+/ref='"$version/" readme.md examples/*/flake.nix
  sed -i 's/hixVersion = ".*"/hixVersion = "'"$version"'"/' modules/basic.nix
  sed -i "s/Unreleased/$version/" changelog.md
  ${config.pkgs.git}/bin/git --no-pager diff
  ${config.pkgs.git}/bin/git add readme.md changelog.md examples modules/basic.nix
  '';

  preamble = ''
  #!${config.pkgs.zsh}/bin/zsh
  setopt err_exit no_unset pipefail

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
  ${config.pkgs.git}/bin/git commit --allow-empty -m "Release $version"
  ${commitAndTag}
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
  ${commitAndTag}
  '';

in { inherit nix all; }
