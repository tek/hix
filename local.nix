{inputs}:
{config, lib, util, ...}: let
  release = import ./lib/release.nix { inherit config util; };
in {
  compiler = "ghc94";
  ghcVersions = [];
  compat.enable = false;

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
      "aeson >= 2.0 && < 2.2"
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
      "optparse-applicative ^>= 0.17"
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
      "aeson >= 2.0 && < 2.2"
      "Cabal"
      "exon >= 1.4 && < 1.7"
      "extra ^>= 1.7"
      "hedgehog >= 1.1 && < 1.3"
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

    apps = let

      tests = import ./test/default.nix { inherit util; };

    in {
      test-basic-1 = util.app tests.sets.test-basic-1;
      test-basic-2 = util.app tests.sets.test-basic-2;
      test-vm = util.app tests.sets.test-vm;
      test-managed = util.app tests.sets.test-managed;
      test = util.app tests.sets.test;

      cli = util.app "${config.outputs.packages.hix}/bin/hix";

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
        '' ''
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

  output = {
    commandApps = false;
    envApps = false;
  };

}
