{ pkgs, keep ? false, ... }:
with builtins;
with pkgs.lib;
let
  test = name: (import (./. + "/${name}/test.nix") { inherit pkgs; }).test;

  tests = {
    basic = test "basic";
    deps = test "deps";
    ghci = test "ghci";
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

  ciSkipTests = "ghci service postgres hackage";

in {
  main = pkgs.writeScript "hix-tests" ''
  #!${pkgs.zsh}/bin/zsh
  hix_dir=$PWD
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

  ghci_match()
  {
    check_match "nix run $1 <<< ':quit'" $2 $3
  }

  runtest()
  {
    setopt local_options err_return
    current="$1"
    test="''${tests[$current]}"
    if [[ -z $test ]]
    then
      message "Invalid test name: $current"
      return 1
    fi
    if [[ -n $CI ]] && [[ -n ''${ci_skip_tests[(r)$current]} ]]
    then
      echo ">>> Skipping test '$current'"
      return 0
    fi
    testdir="$tmpdir/$1"
    cp -r "$hix_dir/test/$1" "$testdir"
    cd "$testdir"
    if [[ -n $(print **/flake.nix(N)) ]]
    then
      sed -i "s#HIX#$hix_dir#" **/flake.nix
      sed -i "s#BASE#$testdir#" **/flake.nix
    fi
    echo ">>> Running test '$current'..."
    source $test
  }

  rm -rf $tmpdir
  mkdir -p $tmpdir

  failure=0
  failed=()
  for target in $=targets
  do
    runtest $target
    if [[ $? != 0 ]]
    then
      echo ">>> Test failed: $target"
      failure=$(( failure + 1 ))
      failed+=($target)
    fi
  done

  if [[ $failure == 0 ]]
  then
    echo '>>> All tests succeeded.'
  else
    echo ">>> $failure tests failed:"
    for t in $failed
    do
      echo " - $t"
    done
    exit 1
  fi
  '';
}
