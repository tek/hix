{config, util}: let

  inherit (util) pkgs;

  git = "${pkgs.git}/bin/git";
  grep = "${pkgs.gnugrep}/bin/grep";
  semver = "${pkgs.semver-tool}/bin/semver";

  preamble = ''
  setopt no_unset
  ${util.loadConsole}

  reset()
  {
    if [[ -n $need_reset ]]
    then
      ${git} reset --quiet --hard
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

  trap abort INT
  trap abort TERM

  ask_abort()
  {
    if ! ask $1
    then
      abort ''${2-}
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

  if [[ ''${1-} == '-n' ]]
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
      ${git} stash push --quiet --include-untracked
      stashed=1
    fi
  else
    if ! ${git} diff --quiet
    then
      ask_abort 'Worktree is dirty. Stage changes and continue?'
      ${git} add .
      ${git} stash push --quiet --keep-index
      stashed=1
    fi
  fi
  need_reset=1
  '';

  updateVersions = ''
  if ask 'Run tests?'
  then
    if ! nix --quiet --quiet --quiet flake show --all-systems >/dev/null
    then
      abort 'Evaluation of outputs failed.'
    fi
    if ! nix run .#test
    then
      abort 'Tests failed.'
    fi
  fi
  if ask 'Build docs?'
  then
    if ! nix build .#docs
    then
      abort 'Docs failed.'
    fi
  fi
  sed -i 's/ref=[^"#]\+/ref='"$version/" readme.md examples/*/flake.nix
  sed -ri 's/~[[:digit:]]+\.[[:digit:]]+\.tar/~'"''${version%.*}.tar/" readme.md
  sed -i 's/hixVersion = ".*"/hixVersion = "'"$version"'"/' modules/basic.nix packages/hix/lib/Hix.hs
  sed -i "s/Unreleased/$version/" changelog.md
  ${git} --no-pager diff
  ask_abort 'Versions updated. Continue?'
  ${git} add .
  '';

  commitAndTag = ''
  if dry_run
  then
    abort 'Dry run, skipping commit.'
  fi
  ask_abort 'Ready to commit. Continue?'
  ${git} commit --allow-empty -m "Release $version"
  ${git} tag -m "Release $version" "$version"
  '';

  grepDevCli = "${grep} --files-with-matches 'hixCli\.dev = true'";

  updateDevCli = "sed -i 's#hixCli\.dev = true#hixCli\.dev = false#'";

  checkDevCliTests = ''
  if ${grepDevCli} local.nix &> /dev/null
  then
    message 'The local flake still uses the development version of the CLI.'
    if ask 'Update?'
    then
      ${updateDevCli} local.nix
      ${git} add local.nix
    else
      ask_abort 'Release anyway?'
    fi
  fi
  dev_cli_test_flakes=($(${grepDevCli} test/**/flake.nix))
  if [[ -n ''${dev_cli_test_flakes-} ]]
  then
    dev_cli_test_names=(''${''${dev_cli_test_flakes#test/}%%/*})
    message 'Some tests still use the development version of the CLI:'
    for dc_test in $dev_cli_test_names
    do
      echo " $(yellow '*') $dc_test"
    done
    if ask 'Update?'
    then
      ${updateDevCli} $=dev_cli_test_flakes
    else
      ask_abort 'Release anyway?'
    fi
    if ask 'Rerun tests?'
    then
      if nix run .#test -- $=dev_cli_test_names
      then
        message 'Tests succeeded, adding updated test flakes.'
      else
        abort 'Tests failed.'
      fi
    fi
    ${git} add $dev_cli_test_flakes
  fi
  '';

  nix = util.zscriptErr "hix-release-nix" ''
  ${preamble}
  ${updateVersions}
  ${checkDevCliTests}
  ${commitAndTag}
  '';

  all = util.zscriptErr "hix-release-all" ''
  ${preamble}
  ${updateVersions}

  if dry_run
  then
    message 'Dry run, skipping CLI release.'
  else
    if ! nix run .#release-source -- -v $version
    then
      abort 'CLI release failed.'
    fi
    # `.#release-source` only adds the cabal config of the uploaded package, not `integration`
    ${git} add packages/*/*.cabal
  fi
  ${checkDevCliTests}
  ${commitAndTag}
  '';

  cliDep = "ops/cli-dep.nix";

  updateCliVersion = let
    url = ''https://hackage.haskell.org/package/hix-''${version}/hix-''${version}.tar.gz'';
  in ''
  (){
    setopt local_options local_traps
    local response_out=$(mktemp --tmpdir hix-update-cli-stdout.XXXXXXXX)
    local error_out=$(mktemp --tmpdir hix-update-cli-stderr.XXXXXXXX)
    trap "rm -f $response_out $error_out" EXIT
    setopt local_options no_err_return
    nix-prefetch-url --unpack ${url} >$response_out 2>$error_out
    local code=$?
    setopt local_options err_return
    if (( $code != 0 ))
    then
      error_message "Fetching hackage hash for ops/cli-dep.nix failed:"
      cat $error_out
      return 1
    fi
    setopt local_options err_return
    local hash=$(tail -n1 $response_out)
    cat > ${cliDep} <<EOF
  {
    version = "$version";
    sha256 = "$hash";
  }
  EOF
    ${git} add ${cliDep}
  }
  '';

  updateCliVersionScript = util.zscript "update-cli-version" ''
  version='${import ../ops/version.nix}'
  ${updateCliVersion}
  '';

in { inherit nix all updateCliVersion updateCliVersionScript; }
