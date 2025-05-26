{self, util}:
let
  inherit (util) pkgs lib;

  testTools = [pkgs.ripgrep pkgs.git pkgs.ansifilter pkgs.gnused pkgs.rsync pkgs.tree];

  keepVars = lib.concatMapStringsSep " " (var: "-k ${var}");

  allowedEnvVars = keepVars [
    # systemd access
    "DBUS_SESSION_BUS_ADDRESS"

    # Writing UTF-8 to stdio from the CLI fails without these
    "LANG"
    "LOCALE_ARCHIVE"

    "_hix_test_system_bin_nix"
    "_hix_test_system_bin_systemd"
    "hix_test_show_stderr"
    "hix_test_show_stderr_failure"
    "hix_test_full_output"
    "hix_test_ci"
    "hix_test_verbose"
    "hix_test_debug"
  ];

  allowedEnvVarsCi = keepVars [
    # These contain config added by actions, like cachix
    "NIX_USER_CONF_FILES"

    # systemd access
    "XDG_RUNTIME_DIR"
  ];

  sharedPreamble = ''
  in_ci()
  {
    [[ -n ''${hix_test_ci-} ]]
  }

  if_ci()
  {
    if in_ci
    then
      eval $@
    fi
  }

  ${util.setupScript { path = testTools; }}
  '';

  preamble = ''
  hix_src_dir=${self}
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
    export LANG=en_US.UTF-8
  '

  _hix_test_bin="$tmp_dir/bin"
  mkdir -p $_hix_test_bin

  cat > $_hix_test_bin/nix << EOF
  #!${pkgs.runtimeShell}
  if [[ -n "\''${hix_nix_quiet:-}" ]]
  then
    exec $_hix_test_system_bin_nix/nix --quiet --quiet --show-trace "\$@"
  else
    exec $_hix_test_system_bin_nix/nix --show-trace "\$@"
  fi
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

  export hix_nix_quiet=1

  # Get microsecond accuracy from the zsh clock
  typeset -F SECONDS
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
  '';

  testWrapper = util.zscriptPure "hix-test-wrapper" ''
  ${sharedPreamble}
  test_base=$1
  test_script=$2
  ${sharedVars}
  step_number=0
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

    local sub() {
      if (( $# > 0 ))
      then
        sed -i "s#\bHIX\b#path:$hix_dir#" "$@"
        sed -i "s#BASE#$work_dir#" "$@"
      fi
    }

    mkdir -p $hix_dir
    rsync -rlt --chmod=u+w --filter='merge ${rsyncFilter}' $hix_src_dir/ $hix_dir/

    if [[ -f $test_config ]]
    then
      local test_config_target="$hix_dir/ops/test-config.nix"
      rsync -lt --chmod=u+w $test_config $test_config_target
      sub $test_config_target
    fi

    rsync -rlt --chmod=u+w ''${test_src}/ ''${work_dir}/
    cd "$work_dir"

    rm test.nix
    sub **/flake.nix(N)

    message "Running test $(bold $(magenta $current))"
    local start_time=$SECONDS
    ${testWrapper} $test_base $test
    local result=$?
    message_hang "$(cyan $(print -f "%.2f" $(( $SECONDS - $start_time )) ) )s"
    return $result
  }
  '';

  main = ''
  local failed=()
  local t
  for t in $targets
  do
    if ! runtest $t
    then
      error_message "Test failed: $t"
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

  local failures=$#failed
  if (( $failures == 0 ))
  then
    message 'All tests succeeded.'
  elif (( $failures > 1 ))
  then
    error_message "$failures tests failed:"
    list_failures
  elif (( $#targets > 1 ))
    error_message "One test failed:"
    list_failures
  then
  fi
  if (( $failures > 0 ))
  then
    exit 1
  fi
  '';

  testAssoc = n: t: "${n} ${t}";

  testsAssocArray = set: lib.concatStringsSep " " (lib.mapAttrsToList testAssoc set);

  defineTargets = set: ''
  if (( $# == 0 ))
  then
    targets=(${toString (lib.attrNames set)})
  else
    targets=($*)
  fi
  typeset -A tests
  set -A tests ${testsAssocArray set}
  '';

  # These flags are available to test cases to control what kinds of automated setup is performed.
  # `updateLock` and `root` default to `true`, since most tests require them.
  defaultCase = {
    # Build inputs for the tests, i.e. packages that should be in `$PATH`.
    path = [];
    # Run `nix flake update` in all subdirectories that contain a `flake.nix`.
    updateLock = true;
    # Change the working directory to `./root` before starting the test, if it exists.
    root = true;
    # Initialize a git repo and create a commit, most likely in `./root` due to the above flag.
    git = false;
    # Run `.#gen-cabal-quiet` before starting the test, but after initializing the git repo.
    genCabal = false;
    # Create a minimal project in `./root`.
    project = false;
  };

  updateLock = ''
  update_lock()
  {
    local flake
    for flake in **/flake.nix(N)
    do
      if [[ $flake == "flake.nix" ]]
      then
        error_message "$(blue 'flake.nix') should not be in the test root."
        return 1
      fi
      pushd ''${flake%/flake.nix}
      step_nix --quiet flake update
      popd
    done
  }
  update_lock
  '';

  projectFlake = pkgs.writeText "flake.nix" ''
  {
    description = "hix test project";

    inputs.hix.url = "HIX";

    outputs = {self, hix}:
    hix.lib._hix_test {
      packages.root.src = ./.;
    };
  }
  '';

  projectMain = pkgs.writeText "Main.hs" ''
  module Main where

  main :: IO ()
  main = putStrLn "success"
  '';

  createProject = ''
  mkdir -p ./root/app
  cp ${projectFlake} ./root/flake.nix
  sed -i "s#HIX#$hix_dir#" ./root/flake.nix
  cp ${projectMain} ./root/app/Main.hs
  '';

  cdRoot = ''
  if [[ -d ./root ]]
  then
    cd root
  fi
  '';

  genCabal = ''
  step_run gen-cabal-quiet
  '';

  gitInit = ''
  print 'result' >> .gitignore
  step git init --quiet
  step git add .
  step git commit -m 'init' --quiet
  '';

  casePreamble = conf:
  lib.optional conf.project createProject
  ++
  lib.optional conf.updateLock updateLock
  ++
  lib.optional conf.root cdRoot
  ++
  lib.optional conf.git gitInit
  ++
  lib.optional conf.genCabal genCabal
  ;

  writeCase = name: caseConf: let
    conf = defaultCase // caseConf;
    casePath = if lib.isFunction conf.path then conf.path pkgs else conf.path;
    path = util.exportPathOptional casePath;
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
    ${sharedPreamble}
    export _hix_test_system_bin_nix=''${$(readlink -f =nix):h}
    export _hix_test_system_bin_systemd=''${$(readlink -f =systemctl):h}
    if [[ -n $hix_test_impure ]]
    then
      exec ${conf.main} $@
    else
      local keep=(${allowedEnvVars})
      if_ci 'keep+=(${allowedEnvVarsCi})'
      exec nix develop \
        --ignore-environment \
        $keep \
        path:${self}#hix-test -c ${conf.main} $@
    fi
    '';

  apps = suites: util.mapValues app (lib.mapAttrs' normalizeSuite suites);

in {
  inherit preamble runtest main defineTargets suite normalizeSuite app apps;
}
