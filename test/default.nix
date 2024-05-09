{util, ...}:
let
  inherit (util) config;
  inherit (config) pkgs;

  tools = import ./tools.nix { inherit util; };

  test = name: (import (./. + "/${name}/test.nix") { inherit config util pkgs; }).test;

  tests-basic-1 = {
    basic = test "basic";
    deps = test "deps";
    ghci = test "ghci";
    packages = test "packages";
    hackage = test "hackage";
    subdir = test "subdir";
    local-prelude = test "local-prelude";
    dep-versions = test "dep-versions";
    update-cli-version = test "update-cli-version";
    direnv = test "direnv";
  };

  tests-basic-2 = {
    cross = test "cross";
    env = test "env";
    new = test "new";
    bootstrap = test "bootstrap";
    new-static = test "new-static";
    new-static-github = test "new-static-github";
    overrides = test "overrides";
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

  tests = tests-basic-1 // tests-basic-2 // tests-vm // tests-managed;

  setup = pkgs.writeText "hix-tests-setup" ''
  ${tools.preamble}
  ${tools.asserts}
  ${tools.runtest}
  '';

  script = name: set: util.zscriptErr "hix-tests-${name}" ''
  source ${setup}
  ${tools.loadTargets set}
  ${tools.main}
  '';

in {
  sets = {
    test-basic-1 = script "basic-1" tests-basic-1;
    test-basic-2 = script "basic-2" tests-basic-2;
    test-vm = script "vm" tests-vm;
    test-managed = script "managed" tests-managed;
    test = script "all" tests;
  };
}
