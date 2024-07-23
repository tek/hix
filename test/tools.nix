{util}:
let
  inherit (util) pkgs lib;

  preamble = ''
  hix_src_dir=$PWD
  tmp_dir=$(mktemp -d --tmpdir hix-test.XXXXXXXX)
  if [[ -z ''${hix_test_keep-} ]]
  then
    trap "rm -rf $tmp_dir" EXIT
  fi

  ${util.loadConsole}

  flake_update()
  {
    nix flake update --quiet --quiet
  }

  if_ci()
  {
    if [[ -n ''${CI-} ]]
    then
      eval $@
    fi
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

  check_note()
  {
    check_eq "$(eval $1)" $2 "Output mismatch for\n    $(bold $(blue $1))\n    Note: $3\n    $(red Output diff)"
  }

  check()
  {
    local msg
    if (( $# < 3 ))
    then
      msg="Output mismatch for $(bold $(blue $1))"
    else
      msg="$3"
    fi
    check_eq "$(eval $1)" $2 $msg
  }

  check_match()
  {
    if (( $# < 3 ))
    then
      msg="No match for $(bold $(green $2))\n    $(red in output of) $(bold $(blue $1))"
    else
      msg="$3"
    fi
    check_re "$(eval $1)" $2 $msg
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

  nonempty()
  {
    setopt local_options err_return
    local file=$1
    [[ -f $file ]]
    local size=$(stat -c %s $file)
    (( $size > 0 ))
  }

  show_output()
  {
    local fd=$1 name=$2
    if nonempty $fd
    then
      message "$(blue $name):"
      echo ""
      echo -e "$(cat $fd)"
      echo ""
    else
      message "$(blue $name) empty"
    fi
  }

  check_exit()
  {
    setopt local_options no_unset
    local output_base=$(mktemp -d "$output_dir/check.XXX")
    local stdout="$output_base/stdout" stderr="$output_base/stderr"
    setopt local_options no_err_return
    eval $1 1>$stdout 2>$stderr
    local code=$?
    if [[ $code != 0 ]]
    then
      error_message "$1 terminated with code $(yellow $code)$(red .)"
      show_output $stdout stdout
      show_output $stderr stderr
      return 1
    fi
  }

  ghci_match()
  {
    check_match "nix run $1 <<< ':quit' 2>&1" $2 $3
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
    local test_base="$tmp_dir/$current"
    local work_dir="$test_base/work"
    local hix_dir="$test_base/hix"
    local output_dir="$test_base/output"
    local test_config="$test_src/test-config.nix"

    if [[ -z $test ]]
    then
      message "Invalid test name: $current"
      return 1
    fi

    mkdir -p $test_base
    mkdir -p $output_dir

    local sub() {
      if (( $# > 0 ))
      then
        sed -i "s#HIX#$hix_dir#" "$@"
        sed -i "s#BASE#$work_dir#" "$@"
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

    cp -r "$test_src" "$work_dir"
    cd "$work_dir"

    sub $(print **/flake.nix(N))

    message "Running test '$current'..."
    source $test
  }
  '';

  main = ''
  rm -rf $tmp_dir
  mkdir -p $tmp_dir

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
