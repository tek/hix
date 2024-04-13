{config, util}: let

  inherit (config.internal) pkgs;

  git = "${pkgs.git}/bin/git";

  preamble = ''
  setopt no_unset pipefail
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

  nix = util.zscript "hix-release-nix" ''
  ${preamble}
  ${updateVersions}
  ${commitAndTag}
  '';

  # TODO updateVersions should probably run after the CLI release so that tests that don't use devCli can validate the
  # new version.
  # Also grep for devCli tests and suggest that they be changed to use the release.
  all = util.zscript "hix-release-all" ''
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

  cliDep = "ops/cli-dep.nix";

  updateCliVersion = let
    url = ''https://hackage.haskell.org/package/hix-''${version}/hix-''${version}.tar.gz'';
  in ''
  response=$({ nix-prefetch-url --unpack ${url} 2>&1 || print 'failed' } | tail -n1)
  if [[ $response == 'failed' ]]
  then
    error_message "Fetching hackage hash for ops/cli-dep.nix failed: $response"
    false
  fi
  cat > ${cliDep} <<EOF
  {
    version = "$version";
    sha256 = "$response";
  }
  EOF
  git add ${cliDep}
  '';

in { inherit nix all updateCliVersion; }
