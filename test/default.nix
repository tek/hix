{self, util, ...}:
let
  inherit (util) config pkgs lib;

  framework = import ./internal/framework.nix { inherit self util; };

  testlib = {
    hackage = import ./internal/hackage.nix { inherit util; };
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
      "ghc-build"
      "ghci"
      "lazy-ifd"
      "lazy-env"
      "local-prelude"
      "module"
      "multi-exe"
      "package-default"
      "packages"
      "prelude-dep"
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
      "init"
      "init-static-github"
      "new"
      "new-parent-dir"
      "new-with-name"
      # "init-static"
      # This is the same as `init` as long as we can't build the CLI statically.
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
      "release"
      "release-version-file"
      "release-workflows"
    ];

  tests-examples =
    testsNamed [
      "example-env-compiler-1"
      "example-env-compiler-2"
      "example-env-compiler-3"
      "example-env-selection"
      "example-flakes-basic"
      "example-flakes-hs"
      "example-old-nixpkgs"
      "example-packages"
    ];

  tests = tests-basic-1 // tests-basic-2 // tests-basic-3 // tests-vm // tests-managed // tests-examples;

  tests-framework = testsNamed ["framework-step"];

  tests-debug = testsNamed ["debug"];

  suites = {
    basic-1 = framework.suite tests-basic-1;
    basic-2 = framework.suite tests-basic-2;
    basic-3 = framework.suite tests-basic-3;
    examples = framework.suite tests-examples;
    managed = framework.suite tests-managed;
    vm = framework.suite tests-vm;
    framework = framework.suite tests-framework;
    debug = framework.suite tests-debug;
    all = framework.suite (tests // tests-framework) // { attr = "test"; };
  };

  ci-tests = util.catSets [
    tests-basic-1
    tests-basic-2
    tests-basic-3
    tests-examples
    tests-managed
    tests-vm
    tests-framework
  ];

in {
  inherit suites;

  legacyPackages = {
    ci-matrix = lib.attrNames ci-tests;
  };

  apps = framework.apps suites;
}
