{ pkgs, keep ? false, ... }:
with builtins;
with pkgs.lib;
let
  test = name: (import (./. + "/${name}/test.nix") { inherit pkgs; }).test;

  tests = {
    deps = test "deps";
    ghci = test "ghci";
    packages = test "packages";
    hackage = test "hackage";
    auto-1 = test "auto-1";
    auto-2 = test "auto-2";
    auto-3 = test "auto-3";
    auto-4 = test "auto-4";
    auto-5 = test "auto-5";
    cross = test "cross";
    service = test "service";
    postgres = test "postgres";
    env = test "env";
  };

  testA = n: t: "${n} ${t}";

  testsA = concatStringsSep " " (mapAttrsToList testA tests);

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

  die()
  {
    echo -e ">>> $*"
    exit 1
  }

  fail()
  {
    echo -e ">>> Test '$current' failed!"
    die $*
  }
  ${if keep then "" else ''trap "rm -rf $tmpdir" EXIT''}

  check()
  {
    output=$(eval $1)
    if [[ $output != $2 ]]
    then
      fail "$3:\n$output"
    fi
  }

  check_match()
  {
    output=$(eval $1)
    if [[ ! $output =~ $2 ]]
    then
      fail "$3:\n$output"
    fi
  }

  runtest()
  {
    setopt local_options err_return
    current="$1"
    test="''${tests[$current]}"
    if [[ -z $test ]]
    then
      die "Invalid test name: $current"
    fi
    testdir="$tmpdir/$1"
    cp -r "$hix_dir/test/$1" "$testdir"
    cd "$testdir"
    sed -i "s#HIX#$hix_dir#" */flake.nix
    sed -i "s#BASE#$testdir#" */flake.nix
    echo ">>> Running test '$current'..."
    source $test
  }

  rm -rf $tmpdir
  mkdir -p $tmpdir

  failure=0
  for target in $=targets
  do
    runtest $target
    if [[ $? != 0 ]]
    then
      echo ">>> Test failed: $target"
      failure=$(( failure + 1 ))
    fi
  done

  if [[ $failure == 0 ]]
  then
    echo '>>> All tests succeeded.'
  else
    echo ">>> $failure tests failed."
    exit 1
  fi
  '';
}
