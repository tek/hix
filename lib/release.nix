{config, util}: let

  inherit (config.internal) pkgs;

  git = "${pkgs.git}/bin/git";
  grep = "${pkgs.gnugrep}/bin/grep";
  semver = "${pkgs.semver-tool}/bin/semver";

  preamble = ''
  setopt no_unset pipefail
  ${util.loadConsole}

  reset()
  {
    if [[ -n $need_reset ]]
    then
      ${git} reset --hard
      if [[ -n $stashed ]]
      then
        ${git} stash pop --quiet --index
      fi
    fi
  }

  abort()
  {
    setopt local_options unset no_pipefail
    reset
    die ''${1:-Aborting.}
  }

  ask_abort()
  {
    if ! ask $1
    then
      if (( $# > 1 ))
      then
        eval $@[1,$]
      fi
      abort
    fi
  }

  dry_run()
  {
    [[ -n ''${hix_release_dry_run-} ]]
  }

  invalid_version()
  {
    message "Invalid version spec $(magenta $1), must be $(green X.Y.Z) or $(green ''${(j., .)valid_args})."
    abort
  }

  if [[ $1 == '-n' ]]
  then
    export hix_release_dry_run=1
    shift
  fi

  current_version="${config.internal.hixVersion}"
  valid_args=(major minor patch)

  if [[ $# == 0 ]]
  then
    message "No version specified, bumping patch."
    version=$(${semver} bump patch $current_version)
  else
    spec="$1"
    if [[ $spec =~ '[\d.]+' ]]
    then
      if [[ $(${semver} validate $spec 2>/dev/null) == 'invalid' ]]
      then
        invalid_version $spec
      elif [[ $(${semver} compare $spec $current_version) != 1 ]]
      then
        message "Specified version $(magenta $spec) isn't newer than current version $(magenta $current_version)."
        abort
      else
        version="$spec"
      fi
    elif [[ -n ''${valid_args[(r)$spec]-} ]]
    then
      version=$(${semver} bump $spec $current_version)
    else
      invalid_version $spec
    fi
  fi
  ask_abort "New version is $(magenta $version). Continue?"

  if dry_run
  then
    if ! ${git} diff --quiet
    then
      ${git} stash push --include-untracked
      stashed=1
    fi
  else
    if ! ${git} diff --quiet
    then
      die 'Worktree is dirty'
    fi
  fi
  need_reset=1
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
  ask_abort 'Versions updated. Continue?' ${git} reset --hard
  ${git} add .
  '';

  commitAndTag = ''
  if dry_run
  then
    message 'Dry run, skipping commit.'
    abort
  fi
  ask_abort 'Ready to commit. Continue?'
  ${git} commit --allow-empty -m "Release $version"
  ${git} tag -m "Release $version" "$version"
  '';

  checkDevCliTests = ''
  dev_cli_test_flakes=($(${grep} --files-with-matches 'hixCli\.dev = true' test/**/flake.nix))
  if [[ -n ''${dev_cli_test_flakes-} ]]
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
    else
      ask_abort 'Release anyway?'
    fi
  fi
  '';

  nix = util.zscript "hix-release-nix" ''
  ${preamble}
  ${updateVersions}
  ${checkDevCliTests}
  ${commitAndTag}
  '';

  all = util.zscript "hix-release-all" ''
  ${preamble}
  ${updateVersions}

  if dry_run
  then
    message 'Dry run, skipping CLI release.'
  else
    nix run .#release -- -v $version
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
