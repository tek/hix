{ util, ... }:
let
  inherit (util) config lib;
  inherit (config) pkgs;

  test = name: (import (./. + "/${name}/test.nix") { inherit config util pkgs; }).test;

  tests-basic = {
    basic = test "basic";
    deps = test "deps";
    ghci = test "ghci";
    packages = test "packages";
    hackage = test "hackage";
    cross = test "cross";
    env = test "env";
    new = test "new";
    bootstrap = test "bootstrap";
    new-static = test "new-static";
    new-static-github = test "new-static-github";
    overrides = test "overrides";
    subdir = test "subdir";
    local-prelude = test "local-prelude";
    dep-versions = test "dep-versions";
    update-cli-version = test "update-cli-version";
    direnv = test "direnv";
  };

  tests-vm = {
    ghci-vm = test "ghci-vm";
    service = test "service";
    postgres = test "postgres";
  };

  tests-managed = {
    bump = test "bump";
    lower = test "lower";
    lower-stabilize = test "lower-stabilize";
    lower-local = test "lower-local";
  };

  tests = tests-basic // tests-vm // tests-managed;

  testAssoc = n: t: "${n} ${t}";

  testsAssocArray = set: lib.concatStringsSep " " (lib.mapAttrsToList testAssoc set);

  rsyncFilter = pkgs.writeText "hix-test-rsync-filter" ''
  + /*.nix
  + /flake.lock
  + /cabal.project
  + /modules/***
  + /lib/***
  + /packages/***
  + /ops/***
  - *
  '';

  preamble = set: ''
  hix_src_dir=$PWD
  tmpdir=/tmp/hix-test-temp
  if [[ -z ''${hix_test_keep-} ]]
  then
    trap "rm -rf $tmpdir" EXIT
  fi
  ${util.loadConsole}

  fail()
  {
    # REMOVE
    # This is only printed when the test has an explicit error condition, not when it's aborted via err_return.
    # But we also print a similar message unconditionally based on the exit status, which this function also triggers.
    # error_message "Test '$current' failed!"
    error_message $*
    return 1
  }

  check_eq()
  {
    if [[ $1 != $2 ]]
    then
      fail "$3:\n$1"
      return 1
    fi
  }

  check_re()
  {
    if [[ ! $1 =~ $2 ]]
    then
      fail "$3:\n$1"
      return 1
    fi
  }

  check()
  {
    check_eq "$(eval $1)" $2 $3
  }

  check_match()
  {
    check_re "$(eval $1)" $2 $3
  }

  check_err()
  {
    setopt local_options no_err_return
    check_eq "$(eval $1 2>&1)" $2 $3
  }

  check_match_err()
  {
    setopt local_options no_err_return
    check_re "$(eval $1 2>&1)" $2 $3
  }

  check_diff()
  {
    if ! diff $1 $2
    then
      fail "$3"
      return 1
    fi
  }

  ghci_match()
  {
    check_match "nix run $1 <<< ':quit'" $2 $3
  }

  flake_update()
  {
    nix flake update --quiet --quiet
  }
  '';

  runtest = ''
  runtest()
  {
    setopt local_options err_return
    local current="$1"
    local test="''${tests[$current]}"
    local test_src="$hix_src_dir/test/$current"
    local test_base="$tmpdir/$current"
    local testdir="$test_base/work"
    local hix_dir="$test_base/hix"
    local test_config="$test_src/test-config.nix"

    if [[ -z $test ]]
    then
      message "Invalid test name: $current"
      return 1
    fi

    mkdir -p $test_base

    local sub() {
      if (( $# > 0 ))
      then
        sed -i "s#HIX#$hix_dir#" "$@"
        sed -i "s#BASE#$testdir#" "$@"
      fi
    }

    mkdir -p $hix_dir
    ${pkgs.rsync}/bin/rsync -rlt --filter='merge ${rsyncFilter}' $hix_src_dir/ $hix_dir/

    if [[ -f $test_config ]]
    then
      local test_config_target="$hix_dir/ops/test-config.nix"
      cp $test_config $test_config_target
      sub $test_config_target
    fi

    cp -r "$test_src" "$testdir"
    cd "$testdir"

    sub $(print **/flake.nix(N))

    message "Running test '$current'..."
    source $test
  }
  '';

  loadTargets = set: ''
  if (( $# == 0 ))
  then
    targets="${toString (lib.attrNames set)}"
  else
    targets="$@"
  fi
  typeset -A tests
  set -A tests ${testsAssocArray set}
  '';

  main = ''
  rm -rf $tmpdir
  mkdir -p $tmpdir

  failure=0
  failed=()
  local t
  for t in $=targets
  do
    runtest $t
    if [[ $? != 0 ]]
    then
      error_message "Test failed: $t"
      (( failure = failure + 1 ))
      failed+=($t)
    fi
  done

  if [[ $failure == 0 ]]
  then
    message 'All tests succeeded.'
  else
    error_message "$failure tests failed:"
    for t in $failed
    do
      error_message " - $t"
    done
    exit 1
  fi
  '';

  script = name: set: pkgs.writeScript "hix-tests-${name}" ''
  #!${pkgs.zsh}/bin/zsh
  ${preamble set}
  ${runtest}
  ${loadTargets set}
  ${main}
  '';

in {
  sets = {
    test-basic = script "basic" tests-basic;
    test-vm = script "vm" tests-vm;
    test-managed = script "managed" tests-managed;
    test = script "all" tests;
  };
}
