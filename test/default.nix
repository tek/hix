{self, util, ...}:
let
  inherit (util) config pkgs lib;

  framework = import ./internal/framework.nix { inherit self util; };

  testlib = {
    hackage = import ./internal/hackage.nix { inherit pkgs; };
  };

  testConf = name: let
    expr = import (./. + "/${name}/test.nix");
  in if lib.isAttrs expr
  then expr
  else expr { inherit config lib util pkgs testlib; inherit (pkgs) system; };

  testsNamed = names: lib.genAttrs names testConf;

  tests-basic-1 =
    testsNamed [
      "basic"
      "command"
      "dep-versions"
      "deps"
      "direnv"
      "gen-cabal"
      "ghci"
      "hackage"
      "hackage-legacy"
      "hackage-local"
      "lazy-ifd"
      "local-prelude"
      "module"
      "multi-exe"
      "package-default"
      "packages"
      "show-config"
      "subdir"
      "update-cli-version"
      "warning"
      "tags"
    ];

  tests-basic-2 =
    testsNamed [
      "cross"
      "env"
      "overrides"
    ];

  tests-basic-3 =
    testsNamed [
      "bootstrap"
      "new"
      "new-with-name"
      "new-parent-dir"
      "init"
      "init-static"
      "init-static-github"
    ];

  tests-vm =
    testsNamed [
    "ghci-vm"
    "postgres"
    "service"
  ];

  tests-managed =
    testsNamed [
      "bump"
      "lower"
      "lower-local"
      "lower-stabilize"
      "maint"
      "maint-workflows"
      "managed-nom"
    ];

  tests-examples =
    testsNamed [
      "example-env-selection"
      "example-flakes-basic"
      "example-flakes-hs"
      "example-packages"
    ];

  tests = tests-basic-1 // tests-basic-2 // tests-basic-3 // tests-vm // tests-managed // tests-examples;

  tests-framework = testsNamed ["framework-step"];

  tests-debug = testsNamed ["debug"];

  suites = {
    basic-1 = framework.suite tests-basic-1;
    basic-2 = framework.suite tests-basic-2;
    basic-3 = framework.suite tests-basic-3;
    vm = framework.suite tests-vm;
    managed = framework.suite tests-managed;
    examples = framework.suite tests-examples;
    framework = framework.suite tests-framework;
    debug = framework.suite tests-debug;
    all = framework.suite (tests // tests-framework) // { attr = "test"; };
  };

  ci-tests = util.catSets [tests-basic-1 tests-basic-2 tests-basic-3 tests-managed tests-vm tests-framework];

in {
  inherit suites;

  legacyPackages = {
    ci-matrix = lib.attrNames ci-tests;
  };

  apps = framework.apps suites;
}
