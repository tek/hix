{config, util}: let

  inherit (config.internal) pkgs;

  git = "${pkgs.git}/bin/git";
  grep = "${pkgs.gnugrep}/bin/grep";

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
    die 'Aborting.'
  fi
  ${git} add .
  '';

  commitAndTag = ''
  if [[ -n ''${hix_release_dry_run-} ]]
  then
    message 'Dry run, skipping commit.'
    exit 0
  fi
  ${git} commit --allow-empty -m "Release $version"
  ${git} tag -m "Release $version" "$version"
  '';

  checkDevCliTests = ''
  dev_cli_test_flakes=($(${grep} --files-with-matches 'hixCli\.dev = true' test/**/flake.nix))
  if [[ -n $dev_cli_test_flakes ]]
  then
    dev_cli_test_names=(''${''${dev_cli_test_flakes#test/}%%/*})
    message 'Some tests still use the development version of the CLI:'
    for dc_test in $dev_cli_test_names
    do
      echo " $(yellow '*') $dc_test"
    done
    if ask 'Update and rerun tests?'
    then
      sed -i 's#hixCli\.dev = true#hixCli\.dev = false#' $=dev_cli_test_flakes
      if nix run .#test -- $=dev_cli_test_names
      then
        message 'Tests succeeded, committing updated test flakes.'
      else
        die 'Tests failed, aborting.'
      fi
      ${git} add $dev_cli_test_flakes
    elif ! ask 'Release anyway?'
    then
      die 'Aborting.'
    fi
  fi
  '';

  nix = util.zscript "hix-release-nix" ''
  ${preamble}
  ${updateVersions}
  ${checkDevCliTests}
  ${commitAndTag}
  '';

  # TODO updateVersions should probably run after the CLI release so that tests that don't use devCli can validate the
  # new version.
  all = util.zscript "hix-release-all" ''
  ${preamble}
  ${updateVersions}

  if [[ -n ''${hix_release_dry_run-} ]]
  then
    message 'Dry run, skipping CLI release.'
  else
    nix run .#release -- -v $version
  fi
  if ! ask 'Update CLI version in overrides. Continue?'
  then
    die 'Aborting.'
  fi
  if ! ${git} add modules/cli.nix
  then
    die 'modules/cli.nix unchanged, aborting.'
  fi
  ${checkDevCliTests}
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
