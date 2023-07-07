{config, lib, util}: let

  inherit (config.internal) pkgs;

  commitAndTag = ''
  ${pkgs.git}/bin/git commit --allow-empty -m "Release $version"
  ${pkgs.git}/bin/git tag -m "Release $version" "$version"
  '';

  updateVersions = ''
  if [[ -z ''${hix_release_skip_test-} ]]
  then
    nix run .#test
  fi
  sed -i 's/ref=[^"#]\+/ref='"$version/" readme.md examples/*/flake.nix
  sed -i 's/hixVersion = ".*"/hixVersion = "'"$version"'"/' modules/basic.nix
  sed -i "s/Unreleased/$version/" changelog.md
  ${pkgs.git}/bin/git --no-pager diff
  ${pkgs.git}/bin/git add readme.md changelog.md examples modules/basic.nix
  '';

  preamble = ''
  #!${pkgs.zsh}/bin/zsh
  setopt err_exit no_unset pipefail

  if [[ $# == 0 ]]
  then
    echo 'Please specify version'
    exit 1
  fi
  version="$1"
  '';

  nix = pkgs.writeScript "hix-release-nix" ''
  ${preamble}
  ${updateVersions}
  ${pkgs.git}/bin/git commit --allow-empty -m "Release $version"
  ${commitAndTag}
  '';

  all = pkgs.writeScript "hix-release-all" ''
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
  ${pkgs.git}/bin/git add modules/cli.nix
  ${commitAndTag}
  '';

in { inherit nix all; }
