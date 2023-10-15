{config, util}: let

  inherit (config.internal) pkgs;

  git = "${pkgs.git}/bin/git";

  preamble = ''
  #!${pkgs.zsh}/bin/zsh
  setopt err_exit no_unset pipefail
  ${util.loadConsole}

  if [[ $# == 0 ]]
  then
    die 'Please specify version'
  fi
  version="$1"

  if ! ${git} diff --quiet
  then
    die 'Worktree is dirty'
  fi

  ask() {
    echo -n "$chevrons $1 [Yn] "
    read -q decision || true
    echo ""
    [[ $decision != 'n' ]]
  }
  '';

  updateVersions = ''
  if ask 'Run tests?'
  then
    nix build .#docs
    nix run .#test
  fi
  sed -i 's/ref=[^"#]\+/ref='"$version/" readme.md examples/*/flake.nix
  sed -Ei 's/~[[:digit:]]+\.[[:digit:]]+\.tar/~'"''${version%.*}.tar/" readme.md
  sed -i 's/hixVersion = ".*"/hixVersion = "'"$version"'"/' modules/basic.nix
  sed -i "s/Unreleased/$version/" changelog.md
  ${git} --no-pager diff
  if ! ask 'Versions updated. Continue?'
  then
    ${git} reset --hard
    die "Aborting."
  fi
  ${git} add .
  '';

  commitAndTag = ''
  ${git} commit --allow-empty -m "Release $version"
  ${git} tag -m "Release $version" "$version"
  '';

  nix = pkgs.writeScript "hix-release-nix" ''
  ${preamble}
  ${updateVersions}
  ${commitAndTag}
  '';

  all = pkgs.writeScript "hix-release-all" ''
  ${preamble}
  ${updateVersions}

  nix run .#release -- -v $version
  if ! ask 'Update CLI version in overrides. Continue?'
  then
    die "Aborting."
  fi
  ${git} add modules/cli.nix
  ${commitAndTag}
  '';

in { inherit nix all; }
