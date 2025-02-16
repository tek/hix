{inputs}:
{config, lib, util, build, ...}: let

  release = import ./lib/release.nix { inherit config util; };

  cabalInstallFix = {
    overrides = {super, ...}: {
      semaphore-compat = null;
      cabal-install = super.cabal-install.overrideScope (cself: csuper: {
        semaphore-compat = null;
      });
    };
  };

in {
  compiler = "ghc98";
  ghcVersions = ["ghc96" "ghc98" "ghc910"];

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
        version = ">=0.5 && <0.7";
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
      "exon >= 1.4 && < 1.8"
      "extra ^>= 1.7"
      "filepattern ^>= 0.1"
      "http-client ^>= 0.7"
      "http-client-tls ^>= 0.3"
      "http-types ^>= 0.12"
      "generic-lens ^>= 2.2"
      "generics-sop ^>= 0.5"
      "lens >= 5.1 && < 5.4"
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
      "exon >= 1.4 && < 1.8"
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

  envs.dev = cabalInstallFix;
  envs.ghc98 = cabalInstallFix;
  envs.ghc910.overrides = {hackage, jailbreak, notest, ...}: {
    exon = hackage "1.7.1.0" "16vf84nnpivxw4a46g7jsy2hg4lpla7grkv3gp8nd69zlv43777l";
    generics-sop = jailbreak (hackage "0.5.1.4" "0ai089kly1cajn4djqnplkg2jmnapqlb3crrsyvfnadcyzc9h3km");
    incipit-base = hackage "0.6.1.0" "0iyyvxpyyybn5ygr875pav6g5hbs00wa9jbr7qslszqpkfpy5x33";
    pcre-heavy = notest;
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

      tests =
        if builtins.pathExists ./test
        then import ./test/default.nix { inherit util; inherit (inputs) self; }
        else { apps = {}; };

      cli = util.app "${build.packages.min.hix.package}/bin/hix";

    in tests.apps // {

      inherit cli;
      default = cli;

      new-nocache = let
        prog = util.bootstrapWithDynamicCli "hix-new-nocache" ''
        $exe new --hix-url '${config.internal.hixUrl}' "$@"
        '' ''
        nix run .#gen-cabal-quiet
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
        nix run .#gen-cabal-quiet
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
