{inputs}:
{config, lib, util, build, ...}: let

  release = import ./lib/release.nix { inherit config util; };

  # In GHC 9.6, `Cabal-syntax` is included in GHC, so the special package `Cabal-syntax_3_10_3_0` is somehow added to
  # the package db twice by `ghcWithPackages`.
  # Works fine for the derivation though.
  cabalHack = env: {
    haskellPackages = env.ghc.ghc.override (old: {
      overrides = util.lib.composeExtensions old.overrides or (_: _: {}) (_: _: {
        Cabal_3_10_3_0 = null;
        Cabal-syntax_3_10_3_0 = null;
      });
    });
  };

in {
  compiler = "ghc98";
  ghcVersions = ["ghc96"];
  compat.enable = false;

  envs.ghc96.ghcWithPackagesArgs = cabalHack config.envs.ghc96;

  hackage = {
    versionFile = "ops/version.nix";
    tag = false;
    formatTag = {name, version}: if name == null then version else "${name}-${version}";
    setChangelogVersion = lib.mkForce false;
    commit = false;
    add = true;
    hooks.postUploadAll = {source, publish}:
    if source && publish
    then release.updateCliVersion
    else "";
  };

  cabal = {
    prelude = {
      enable = true;
      package = {
        name = "incipit-base";
        version = "^>= 0.5";
      };
      module = "IncipitBase";
    };
    paths = false;
  };

  packages.hix = {
    src = ./packages/hix;

    cabal = {

      license = "BSD-2-Clause-Patent";
      license-file = "LICENSE";
      author = "Torsten Schmits";

      meta = {
        maintainer = "hackage@tryp.io";
        category = "Build";
        git = "https://git.tryp.io/tek/hix";
        homepage = "https://git.tryp.io/tek/hix";
        bug-reports = "https://github.com/tek/hix/issues";
        synopsis = "Haskell/Nix development build tools";
      };

    };

    library.enable = true;
    library.dependencies = [
      "Cabal"
      "cabal-install"
      "cabal-install-solver"
      "aeson >= 2.0 && < 2.3"
      "bytestring"
      "casing ^>= 0.1.4"
      "containers"
      "exceptions ^>= 0.10"
      "exon >= 1.4 && < 1.7"
      "extra ^>= 1.7"
      "filepattern ^>= 0.1"
      "http-client ^>= 0.7"
      "http-client-tls ^>= 0.3"
      "http-types ^>= 0.12"
      "generic-lens ^>= 2.2"
      "generics-sop ^>= 0.5"
      "lens >= 5.1 && < 5.3"
      "lens-regex-pcre ^>= 1.1"
      "optparse-applicative >= 0.17 && <0.19"
      "path ^>= 0.9"
      "path-io >= 1.7 && < 1.9"
      "pretty"
      "random ^>= 1.2"
      "these ^>=1.2"
      "time"
      "typed-process ^>= 0.2"
      "transformers"
      "unix"
    ];

    executable.enable = true;

    test.enable = true;
    test.dependencies = [
      "aeson >= 2.0 && < 2.3"
      "Cabal"
      "exon >= 1.4 && < 1.7"
      "extra ^>= 1.7"
      "hedgehog >= 1.1 && < 1.5"
      "path ^>= 0.9"
      "path-io >= 1.7 && < 1.9"
      "tasty ^>= 1.4"
      "tasty-hedgehog >= 1.3 && < 1.5"
      "these ^>=1.2"
      "transformers"
    ];

  };

  internal.cabal-extra.default-extensions = ["StrictData"];

  outputs = {

    packages = let
      docs = import ./lib/doc/default.nix { inherit inputs; inherit (config) pkgs; inherit (config.internal) hixUrl; };
    in {
      docs = docs.html;
    };

    devShells.hix-test = config.pkgs.mkShell {};

    apps = let

      tests = import ./test/default.nix { inherit util; };

    in {
      test-basic-1 = tests.apps.test-basic-1;
      test-basic-2 = tests.apps.test-basic-2;
      test-basic-3 = tests.apps.test-basic-3;
      test-vm = tests.apps.test-vm;
      test-managed = tests.apps.test-managed;
      test = tests.apps.test;

      test-framework = tests.apps.test-framework;

      cli = util.app "${build.packages.min.hix.package}/bin/hix";

      new-nocache = let
        prog = util.bootstrapWithDynamicCli "hix-new-nocache" ''
        $exe new --hix-url '${config.internal.hixUrl}' "$@"
        '' ''
        ${util.nixC} run .#gen-cabal-quiet
        '';
      in util.app prog;

      new = let
        prog = util.script "hix-new" ''${util.nixC} run ${inputs.self}#new-nocache -- "$@"'';
      in util.app prog;

      bootstrap-nocache = let
        prog = util.bootstrapWithDynamicCli "hix-bootstrap-nocache" ''
        $exe bootstrap --hix-url '${config.internal.hixUrl}' "$@"
        ''
        ''
        ${util.nixC} run .#gen-cabal-quiet
        '';
      in util.app prog;

      bootstrap = let
        prog = util.script "hix-bootstrap" ''${util.nixC} run ${inputs.self}#bootstrap-nocache -- "$@"'';
      in util.app prog;

      release-nix = util.app release.nix;

      release-all = util.app release.all;

    };

  };

}
