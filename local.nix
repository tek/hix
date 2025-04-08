{inputs}:
{config, lib, util, build, ...}: let

  release = import ./lib/release.nix { inherit config util; };

in {
  compiler = "ghc98";
  ghcVersions = ["ghc96" "ghc98" "ghc910"];
  main = "hix";

  packages.hix = {
    src = ./packages/hix;

    buildInputs = p: [p.git];

    library = {
      enable = true;
      dependencies = [
        "Cabal"
        "aeson >= 2.0 && < 2.3"
        "bytestring"
        "cabal-install"
        "cabal-install-solver"
        "casing ^>= 0.1.4"
        "containers"
        "exceptions ^>= 0.10"
        "exon >= 1.4 && < 1.8"
        "extra ^>= 1.7"
        "filepattern ^>= 0.1"
        "generic-lens ^>= 2.2"
        "generics-sop ^>= 0.5"
        "http-client ^>= 0.7"
        "http-client-tls ^>= 0.3"
        "http-types ^>= 0.12"
        "generic-lens ^>= 2.2"
        "generics-sop ^>= 0.5"
        "lens >= 5.1 && < 5.4"
        "lens-regex-pcre ^>= 1.1"
        "monad-control ^>= 1.0"
        "network"
        "network-uri"
        "optparse-applicative >= 0.17 && <0.19"
        "path ^>= 0.9"
        "path-io >= 1.7 && < 1.9"
        "pretty"
        "random ^>= 1.2"
        "template-haskell"
        "these ^>=1.2"
        "time"
        "transformers"
        "typed-process ^>= 0.2"
        "unix"
      ];
    };

    executable.enable = true;

    test = {
      enable = true;
      dependencies = [
        "Cabal"
        "aeson >= 2.0 && < 2.3"
        "exon >= 1.4 && < 1.8"
        "extra ^>= 1.7"
        "hedgehog >= 1.1 && < 1.5"
        "path ^>= 0.9"
        "path-io >= 1.7 && < 1.9"
        "pretty"
        "tasty ^>= 1.4"
        "tasty-hedgehog >= 1.3 && < 1.5"
        "these ^>=1.2"
        "time"
        "transformers"
      ];
    };

  };

  packages.integration = {
    src = ./packages/integration;

    library = {
      enable = true;
      dependencies = [
        "aeson >= 2.0 && < 2.3"
        "async"
        "casing"
        "exon >= 1.4 && < 1.7"
        "exceptions"
        "hackage-server"
        "network-uri"
        "optparse-applicative >= 0.17 && <0.19"
        "path ^>= 0.9"
        "path-io >= 1.7 && < 1.9"
        config.packages.hix.dep.exact
      ];
    };

    test = {
      enable = true;
      dependencies = [
        "aeson >= 2.0 && < 2.3"
        "Cabal"
        "exon >= 1.4 && < 1.7"
        "extra ^>= 1.7"
        "hedgehog >= 1.1 && < 1.5"
        "lens >= 5.1 && < 5.4"
        "path ^>= 0.9"
        "path-io >= 1.7 && < 1.9"
        "tasty ^>= 1.4"
        "tasty-hedgehog >= 1.3 && < 1.5"
        "time"
        "transformers"
        config.packages.hix.dep.exact
      ];
      env = "integration";
    };

    executable.enable = true;

    expose = false;

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

  commands.integration-hackage = {
    env = "integration";
    command = ''
    ${build.packages.integration.integration.package}/bin/integration hackage $@
    '';
    expose = true;
  };

  commands.hls.env = lib.mkForce "integration";

  envs = let

    cabalInstallFix = {
      overrides = {super, ...}: {
        cabal-install = super.cabal-install.overrideScope (cself: csuper: {
          semaphore-compat = null;
        });
      };
    };

    hackageServer = config.pkgs.fetchFromGitea {
      domain = "git.tryp.io";
      owner = "tek";
      repo = "hackage-server";
      sha256 = "sha256-bm9jpskrYM4QKbP5tjdBy3BemSZnMH/Tx2yUuvuhr8c=";
      rev = "678022773f7cd7db9264d8f9bf275e0ead9ea28a";
    };

  in {

    integration = {
      expose = false;
      packages = ["hix" "integration"];
      env = {
        hackage_data_dir = "${hackageServer}/datafiles";
        hix_dir = "${inputs.self}";
      };

      overrides = api@{hackage, fast, source, force, self, minimal, overrideAttrs, ...}:
      cabalInstallFix.overrides api // {
        hix = minimal;
        integration = fast (overrideAttrs (old: { hackage_data_dir = "${hackageServer}/datafiles"; }));
        hackage-security = self.hackage-security_0_6_2_6;
        hackage-server = force (source.root hackageServer);
        tar = hackage "0.6.3.0" "02nq0l9bsnkk5w8lbp493anc01fyf45l7zbcahhzji02agjwxkqm";
      };

    };

    dev = cabalInstallFix;

    ghc98 = cabalInstallFix;

    ghc910.overrides = {hackage, jailbreak, notest, ...}: {
      exon = hackage "1.7.1.0" "16vf84nnpivxw4a46g7jsy2hg4lpla7grkv3gp8nd69zlv43777l";
      generics-sop = jailbreak (hackage "0.5.1.4" "0ai089kly1cajn4djqnplkg2jmnapqlb3crrsyvfnadcyzc9h3km");
      incipit-base = hackage "0.6.1.0" "0iyyvxpyyybn5ygr875pav6g5hbs00wa9jbr7qslszqpkfpy5x33";
      pcre-heavy = notest;
    };

  };

  internal.cabal-extra.default-extensions = ["StrictData"];

  outputs = let

    # The test runner does not copy `./test` to the temporary directory.
    # When the flake is evaluated in a test, this import would cause an exception without a guard.
    tests =
      if builtins.pathExists ./test
      then import ./test/default.nix { inherit util; inherit (inputs) self; }
      else { apps = {}; legacyPackages = {}; };

  in {

    packages = let
      docs = import ./lib/doc/default.nix { inherit inputs; inherit (config) pkgs; inherit (config.internal) hixUrl; };
    in {
      docs = docs.html;
    };

    devShells.hix-test = config.pkgs.mkShell {};

    apps = let

      cli = util.app "${build.packages.min.hix.package}/bin/hix";

      create-nocache = name: let
        prog = util.bootstrapWithDynamicCli "hix-${name}-nocache" ''
        $exe ${name} --hix-url '${config.internal.hixUrl}' "$@"
        '';
      in util.app prog;

      create = name: let
        prog = util.script "hix-${name}" ''${util.nixC} run ${inputs.self}#${name}-nocache -- "$@"'';
      in util.app prog;

    in tests.apps // {

      inherit cli;
      default = cli;

      init-nocache = create-nocache "init";

      init = create "init";

      new-nocache = create-nocache "new";

      new = create "new";

      bootstrap-nocache = create-nocache "bootstrap";

      bootstrap = create "bootstrap";

      release-nix = util.app release.nix;

      release-all = util.app release.all;

    };

    legacyPackages = tests.legacyPackages;

  };

}
