{util}:
let
  inherit (util) pkgs lib;

  preamble = ''
  hix_src_dir=$PWD
  tmpdir=/tmp/hix-test-temp
  if [[ -z ''${hix_test_keep-} ]]
  then
    trap "rm -rf $tmpdir" EXIT
  fi
  ${util.loadConsole}
  flake_update()
  {
    nix flake update --quiet --quiet
  }
  '';

  asserts = ''
  fail()
  {
    error_message $*
    return 1
  }

  check_eq()
  {
    setopt local_options no_err_return
    if [[ $1 != $2 ]]
    then
      fail "$3:"
      print ""
      diff <(print $1) <(print $2)
      print ""
      print "< $(red 'actual')
  ---
  > $(green 'expected')"
      return 1
    fi
  }

  check_re()
  {
    if [[ ! $1 =~ $2 ]]
    then
      fail "$3:\n$1"
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

  check_exit()
  {
    setopt local_options no_err_return no_unset
    out=$(eval $1 2>&1)
    if [[ $? != 0 ]]
    then
      fail "$2: $out"
      return 1
    fi
  }

  ghci_match()
  {
    check_match "nix run $1 <<< ':quit'" $2 $3
  }
  '';

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

  main = ''
  rm -rf $tmpdir
  mkdir -p $tmpdir

  local failure=0
  local failed=()
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

  testAssoc = n: t: "${n} ${t}";

  testsAssocArray = set: lib.concatStringsSep " " (lib.mapAttrsToList testAssoc set);

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

in {
  inherit preamble asserts runtest main loadTargets;
}
