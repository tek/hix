{util, ...}:
let
  inherit (util) config;
  inherit (config) pkgs;

  framework = import ./internal/framework.nix { inherit util; };

  test = name: let
    conf = import (./. + "/${name}/test.nix") { inherit config util pkgs; };
  in
  if conf ? source
  then pkgs.writeText "hix-test-${name}" conf.source
  else conf.test;

  tests-basic-1 = {
    app-rec = test "app-rec";
    basic = test "basic";
    dep-versions = test "dep-versions";
    deps = test "deps";
    direnv = test "direnv";
    ghci = test "ghci";
    hackage = test "hackage";
    local-prelude = test "local-prelude";
    multi-exe = test "multi-exe";
    packages = test "packages";
    subdir = test "subdir";
    update-cli-version = test "update-cli-version";
  };

  tests-basic-2 = {
    cross = test "cross";
    env = test "env";
    overrides = test "overrides";
  };

  tests-basic-3 = {
    bootstrap = test "bootstrap";
    new = test "new";
    new-static = test "new-static";
    new-static-github = test "new-static-github";
  };

  tests-vm = {
    ghci-vm = test "ghci-vm";
    postgres = test "postgres";
    service = test "service";
  };

  tests-managed = {
    bump = test "bump";
    lower = test "lower";
    lower-local = test "lower-local";
    lower-stabilize = test "lower-stabilize";
    managed-nom = test "managed-nom";
  };

  tests = tests-basic-1 // tests-basic-2 // tests-basic-3 // tests-vm // tests-managed;

  tests-framework = {
    framework-step = test "framework-step";
  };

  script = name: set: util.zscriptPure "hix-tests-${name}" ''
  ${framework.preamble}
  ${framework.runtest}
  ${framework.loadTargets set}
  ${framework.main}
  '';

  testApp = main: util.zapp "hix-test" ''
  export _hix_test_system_bin_nix=''${$(readlink -f =nix):h}
  export _hix_test_system_bin_systemd=''${$(readlink -f =systemctl):h}
  if [[ -n $hix_test_impure ]]
  then
  ${main} $@
  else
    nix develop --ignore-environment \
      -k DBUS_SESSION_BUS_ADDRESS \
      -k _hix_test_system_bin_nix \
      -k _hix_test_system_bin_systemd \
      -k hix_test_show_stderr \
      -k hix_test_show_stderr_failure \
      path:.#hix-test -c ${main} $@
  fi
  '';

  sets = {
    test-basic-1 = script "basic-1" tests-basic-1;
    test-basic-2 = script "basic-2" tests-basic-2;
    test-basic-3 = script "basic-3" tests-basic-3;
    test-vm = script "vm" tests-vm;
    test-managed = script "managed" tests-managed;
    test = script "all" tests;

    test-framework = script "framework" tests-framework;
  };

in {
  inherit sets;

  apps = util.mapValues testApp sets;
}
