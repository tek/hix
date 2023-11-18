{inputs, hix}:

hix.pro [({config, lib, util, ...}: let
  pkgs = config.pkgs;

  release = import ./lib/release.nix { inherit config util; };
in {
  compiler = "ghc94";

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

  overrides = {hackage, ...}: {
    exon = hackage "1.6.0.1" "0wnjywsxsmfqhyymzxlk8zzc5k4jr15y8rgl3lcdw48jl80i6ix9";
  };
  gen-overrides.enable = true;

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
      "lens >= 5.1 && < 5.3"
      "lens-regex-pcre ^>= 1.1"
      "optparse-applicative ^>= 0.17"
      "path ^>= 0.9"
      "path-io >= 1.7 && < 1.9"
      "pretty"
      "random ^>= 1.2"
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
      "transformers"
    ];

  };

  outputs = {

    packages = let
      docs = import ./lib/doc/default.nix { inherit inputs; inherit (config) pkgs; inherit (config.internal) hixUrl; };
    in {
      docs = docs.html;
    };

    apps = let
      tests = import ./test/default.nix { inherit util; };
    in {

      test = {
        type = "app";
        program = "${tests.main}";
      };

      cli = {
        type = "app";
        program = "${config.outputs.packages.hix}/bin/hix";
      };

      new-nocache = let
        prog = util.bootstrapWithDynamicCli "hix-new-nocache" ''
        $exe new --hix-url '${config.internal.hixUrl}' "$@"
        '' ''
        ${util.nixC} run .#gen-cabal-quiet
        '';
      in {
        type = "app";
        program = "${prog}";
      };

      new = let
        prog = pkgs.writeScript "hix-new" ''${util.nixC} run ${inputs.self}#new-nocache -- "$@"'';
      in {
        type = "app";
        program = "${prog}";
      };

      bootstrap-nocache = let
        prog = util.bootstrapWithDynamicCli "hix-bootstrap-nocache" ''
        $exe bootstrap --hix-url '${config.internal.hixUrl}' "$@"
        '' ''
        ${util.nixC} run .#gen-cabal-quiet
        '';
      in {
        type = "app";
        program = "${prog}";
      };

      bootstrap = let
        prog = pkgs.writeScript "hix-bootstrap" ''${util.nixC} run ${inputs.self}#bootstrap-nocache -- "$@"'';
      in {
        type = "app";
        program = "${prog}";
      };

      release-nix = {
        type = "app";
        program = "${release.nix}";
      };

      release-all = {
        type = "app";
        program = "${release.all}";
      };

    };

  };

  output = {
    commandApps = false;
    envApps = false;
  };

}) (import ./ops/test-config.nix)]
