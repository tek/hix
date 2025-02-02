{self, util}:
let
  inherit (util) pkgs lib;

  testTools = [pkgs.ripgrep pkgs.git pkgs.ansifilter pkgs.gnused];

  sharedPreamble = ''
  if_ci()
  {
    if [[ -n ''${hix_test_ci-} ]]
    then
      eval $@
    fi
  }

  ${util.setupScript {path = testTools;}}
  '';

  preamble = ''
  hix_src_dir=$PWD
  tmp_dir=$(mktemp -d --tmpdir hix-test.XXXXXXXX)
  mkdir -p $tmp_dir
  export HOME=$tmp_dir

  _remove_tmp_dir()
  {
    setopt local_options no_err_return
    if [[ -e $tmp_dir ]]
    then
      rm -rf $tmp_dir
    fi
  }

  if [[ -z ''${hix_test_keep-} ]]
  then
    trap _remove_tmp_dir EXIT
  fi

  ${sharedPreamble}

  if_ci '
    export hix_test_full_output=1
    export hix_test_show_stderr_failure=1
    export LC_ALL="C.utf8"
    export LANG="C.utf8"
  '

  _hix_test_bin="$tmp_dir/bin"
  mkdir -p $_hix_test_bin

  cat > $_hix_test_bin/nix << EOF
  #!/bin/sh
  exec $_hix_test_system_bin_nix/nix --quiet --quiet --show-trace "\$@"
  EOF

  cat > $_hix_test_bin/nix-vanilla << EOF
  #!/bin/sh
  exec $_hix_test_system_bin_nix/nix "\$@"
  EOF

  chmod +x $_hix_test_bin/*

  export PATH="$_hix_test_bin:$_hix_test_system_bin_nix:$_hix_test_system_bin_systemd:$PATH"

  export GIT_CONFIG_NOSYSTEM=1
  git config --global user.name hix-test
  git config --global user.email hix-test@localhost
  '';

  asserts = ''
  flake_update()
  {
    nix flake update
  }

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

  sharedVars = ''
  local work_dir="$test_base/work"
  local hix_dir="$test_base/hix"
  local output_dir="$test_base/output"
  '';

  testWrapper = util.zscriptPure "hix-test-wrapper" ''
  ${sharedPreamble}
  test_base=$1
  test_script=$2
  ${sharedVars}
  step_number=0
  ${asserts}
  source ${./step.zsh}
  _hix_test_scope()
  {
    setopt local_options err_return
    source $test_script
  }
  _hix_test_scope
  '';

  runtest = ''
  runtest()
  {
    setopt local_options err_return
    local current="$1"
    local test="''${tests[$current]}"
    local test_src="$hix_src_dir/test/$current"
    local test_base="$tmp_dir/$current"
    local test_config="$test_src/test-config.nix"
    ${sharedVars}

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

    rm test.nix
    sub $(print **/flake.nix(N))

    message "Running test $(bold $(magenta $current))"
    ${testWrapper} $test_base $test
  }
  '';

  main = ''
  local failure=0
  local failed=()
  local t
  for t in $=targets
  do
    if ! runtest $t
    then
      error_message "Test failed: $t"
      (( failure ++ ))
      failed+=($t)
    fi
  done

  list_failures()
  {
    for t in $failed
    do
      message " $(yellow '*') $(bold $(magenta $t))"
    done
  }

  if (( $failure == 0 ))
  then
    message 'All tests succeeded.'
  elif (( $failure > 1 ))
  then
    error_message "$failure tests failed:"
    list_failures
  elif (( $#targets > 1 ))
    error_message "One test failed:"
    list_failures
  then
  fi
  if (( $failure > 0 ))
  then
    exit 1
  fi
  '';

  testAssoc = n: t: "${n} ${t}";

  testsAssocArray = set: lib.concatStringsSep " " (lib.mapAttrsToList testAssoc set);

  defineTargets = set: ''
  if (( $# == 0 ))
  then
    targets="${toString (lib.attrNames set)}"
  else
    targets="$@"
  fi
  typeset -A tests
  set -A tests ${testsAssocArray set}
  '';

  # TODO run common steps like flake_update based on flags on the set.
  casePreamble  = conf: [];

  writeCase = name: conf: let
    path = util.exportPathOptional (conf.path or []);
    test = conf.source or "source ${conf.test}";
  in pkgs.writeText "hix-test-${name}" (util.unlines (path ++ casePreamble conf ++ [test]));

  suite = conf: {
    script = ''
    ${preamble}
    ${runtest}
    ${defineTargets (lib.mapAttrs writeCase conf)}
    ${main}
    '';
  };

  normalizeSuite = suiteName: conf: {
    # The flake attribute in `apps`
    name = conf.attr or "test-${suiteName}";
    value = {
      inherit suiteName;
      main = util.zscriptPure "hix-test-suite-${suiteName}" conf.script;
    };
  };

  app = conf:
    util.zapp "hix-test-app-${conf.suiteName}" ''
    export _hix_test_system_bin_nix=''${$(readlink -f =nix):h}
    export _hix_test_system_bin_systemd=''${$(readlink -f =systemctl):h}
    if [[ -n $hix_test_impure ]]
    then
    exec ${conf.main} $@
    else
      exec nix develop \
        --ignore-environment \
        -k DBUS_SESSION_BUS_ADDRESS \
        -k _hix_test_system_bin_nix \
        -k _hix_test_system_bin_systemd \
        -k hix_test_show_stderr \
        -k hix_test_show_stderr_failure \
        -k hix_test_full_output \
        -k hix_test_ci \
        -k hix_test_verbose \
        path:${self}#hix-test -c ${conf.main} $@
    fi
    '';

  apps = suites: util.mapValues app (lib.mapAttrs' normalizeSuite suites);

in {
  inherit preamble runtest main defineTargets suite normalizeSuite app apps;
}
