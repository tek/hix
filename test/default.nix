{self, util, ...}:
let
  inherit (util) config pkgs lib;

  framework = import ./internal/framework.nix { inherit self util; };

  testlib = {
    hackage = import ./internal/hackage.nix { inherit pkgs; };
  };

  test = name: let
    expr = import (./. + "/${name}/test.nix");
  in if lib.isAttrs expr
  then expr
  else expr { inherit config util pkgs testlib; };

  tests-basic-1 = {
    app-rec = test "app-rec";
    basic = test "basic";
    dep-versions = test "dep-versions";
    deps = test "deps";
    direnv = test "direnv";
    gen-cabal = test "gen-cabal";
    ghci = test "ghci";
    hackage = test "hackage";
    hackage-legacy = test "hackage-legacy";
    local-prelude = test "local-prelude";
    multi-exe = test "multi-exe";
    package-default = test "package-default";
    packages = test "packages";
    subdir = test "subdir";
    update-cli-version = test "update-cli-version";
    warning = test "warning";
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

  suites = {
    basic-1 = framework.suite tests-basic-1;
    basic-2 = framework.suite tests-basic-2;
    basic-3 = framework.suite tests-basic-3;
    vm = framework.suite tests-vm;
    managed = framework.suite tests-managed;
    all = framework.suite tests // { attr = "test"; };
    framework = framework.suite tests-framework;
  };

in {
  inherit suites;

  apps = framework.apps suites;
}
