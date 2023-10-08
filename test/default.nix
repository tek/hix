{ pkgs, keep ? false, ... }:
with builtins;
with pkgs.lib;
let
  test = name: (import (./. + "/${name}/test.nix") { inherit pkgs; }).test;

  tests = {
    basic = test "basic";
    deps = test "deps";
    ghci = test "ghci";
    ghci-vm = test "ghci-vm";
    packages = test "packages";
    hackage = test "hackage";
    cross = test "cross";
    service = test "service";
    postgres = test "postgres";
    env = test "env";
    new = test "new";
    bootstrap = test "bootstrap";
    new-static = test "new-static";
    new-static-github = test "new-static-github";
    overrides = test "overrides";
    subdir = test "subdir";
    local-prelude = test "local-prelude";
    dep-versions = test "dep-versions";
  };

  testA = n: t: "${n} ${t}";

  testsA = concatStringsSep " " (mapAttrsToList testA tests);

  ciSkipTests = "ghci-vm service postgres hackage";

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

in {
  main = pkgs.writeScript "hix-tests" ''
  #!${pkgs.zsh}/bin/zsh
  hix_src_dir=$PWD
  tmpdir=/tmp/hix-test-temp
  if (( $# == 0 ))
  then
    targets="${toString (attrNames tests)}"
  else
    targets="$@"
  fi
  typeset -A tests
  set -A tests ${testsA}
  typeset -a vm_tests
  ci_skip_tests=(${ciSkipTests})

  message()
  {
    echo -e ">>> $*"
  }

  fail()
  {
    message "Test '$current' failed!"
    message $*
    return 1
  }

  ${if keep then "" else ''
  if [[ -z ''${hix_test_keep-} ]]
  then
    trap "rm -rf $tmpdir" EXIT
  fi
  ''}

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

  check_run()
  {
    check "$(eval $1)" $2 $3
  }

  ghci_match()
  {
    check_match "nix run $1 <<< ':quit'" $2 $3
  }

  flake_update()
  {
    nix flake update --quiet --quiet
  }

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

    if [[ -n $CI ]] && [[ -n ''${ci_skip_tests[(r)$current]} ]]
    then
      message "Skipping test '$current'"
      return 0
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
      message "Test failed: $t"
      (( failure = failure + 1 ))
      failed+=($t)
    fi
  done

  if [[ $failure == 0 ]]
  then
    message 'All tests succeeded.'
  else
    message "$failure tests failed:"
    for t in $failed
    do
      echo " - $t"
    done
    exit 1
  fi
  '';
}
