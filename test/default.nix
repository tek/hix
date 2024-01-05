{util, ...}:
let
  inherit (util) config;
  inherit (config) pkgs;

  tools = import ./tools.nix { inherit util; };

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

  setup = pkgs.writeText "hix-tests-setup" ''
  ${tools.preamble}
  ${tools.asserts}
  ${tools.runtest}
  '';

  script = name: set: pkgs.writeScript "hix-tests-${name}" ''
  #!${pkgs.zsh}/bin/zsh
  source ${setup}
  ${tools.loadTargets set}
  ${tools.main}
  '';

in {
  sets = {
    test-basic = script "basic" tests-basic;
    test-vm = script "vm" tests-vm;
    test-managed = script "managed" tests-managed;
    test = script "all" tests;
  };
}
