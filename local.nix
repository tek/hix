{inputs}:
{config, lib, util, build, ...}: let

  release = import ./lib/release.nix { inherit config util; };

in {
  compiler = "ghc912";
  ghcVersions = ["ghc912"];
  main = "hix";
  gen-overrides.enable = true;

  packages.hix = ({config, ...}: {
    src = ./packages/hix;

    buildInputs = p: [p.git];

    library = {
      enable = true;
      dependencies = [
        "Cabal"
        "aeson"
        "bytestring"
        "cabal-install"
        "cabal-install-solver"
        "casing"
        "containers"
        "exceptions"
        "exon"
        "extra"
        "filepattern"
        "generic-lens"
        "generics-sop"
        "http-client"
        "http-client-tls"
        "http-types"
        "generic-lens"
        "generics-sop"
        "lens"
        "lens-regex-pcre"
        "monad-control"
        "network"
        "network-uri"
        "optparse-applicative"
        "path"
        "path-io"
        "pretty"
        "random"
        "template-haskell"
        "these"
        "time"
        "transformers"
        "typed-process"
        "unix"
      ];
    };

    libraries.testing = {
      public = true;
      dependencies = [
        "Cabal"
        "exon"
        "extra"
        "hedgehog"
        "path"
        "path-io"
        "tasty"
        "tasty-hedgehog"
        config.library.dep.exact
      ];
    };

    executable.enable = true;

    test = {
      enable = true;
      dependencies = [
        "Cabal"
        "aeson"
        "exon"
        "extra"
        "hedgehog"
        "path"
        "path-io"
        "pretty"
        "tasty"
        "tasty-hedgehog"
        config.libraries.testing.dep.exact
        "these"
        "time"
        "transformers"
      ];
    };

    override = {nodoc, ...}: nodoc;

  });

  packages.integration = {
    src = ./packages/integration;

    library = {
      enable = true;
      dependencies = [
        "aeson"
        "async"
        "casing"
        "exon"
        "exceptions"
        "hackage-server"
        "network-uri"
        "optparse-applicative"
        "path"
        "path-io"
        config.packages.hix.dep.exact
      ];
    };

    test = {
      enable = true;
      dependencies = [
        "aeson"
        "Cabal"
        "exon"
        "hedgehog"
        "lens"
        "path"
        "path-io"
        "tasty"
        "time"
        "transformers"
        config.packages.hix.dep.exact
        config.packages.hix.libraries.testing.dep.exact
      ];
      env = "integration";
    };

    executable.enable = true;

    expose = false;

  };

  cabal = {
    prelude = {
      enable = true;
      package = "incipit-base";
      module = "IncipitBase";
    };
    paths = false;

    license = "BSD-2-Clause-Patent";
    license-file = "LICENSE";
    author = "Torsten Schmits";
    language = "GHC2021";

    meta = {
      maintainer = "hackage@tryp.io";
      category = "Build";
      github = "tek/hix";
      synopsis = "Haskell/Nix development build tools";
    };
  };

  hackage = {
    versionFile = "ops/version.nix";
    packages = ["hix"];
    tag = false;
    setChangelogVersion = lib.mkForce false;
    commit = false;
    add = true;
    hooks.postUploadAll = {source, publish}:
    if source && publish
    then release.updateCliVersion
    else "";
  };

  managed = {
    enable = true;
    lower.enable = true;
    latest.compiler = "ghc912";
    lower.compiler = "ghc98";
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

    hackageServer = config.pkgs.fetchFromGitea {
      domain = "git.tryp.io";
      owner = "tek";
      repo = "hackage-server";
      sha256 = "sha256-3eZ+rLpDYwPufTr32mO8qXOGfTzOjHAJY9A6Mhhdxpw=";
      rev = "02b91400b55671be79926def26646f7c72388566";
    };

  in {

    integration = {
      expose = false;
      packages = ["hix" "integration"];
      env = {
        hackage_data_dir = "${hackageServer}/datafiles";
        hix_dir = "${inputs.self}";
      };

      overrides = {hackage, fast, source, force, self, minimal, notest, overrideAttrs, ...}: {
        Cabal = hackage "3.16.0.0" "1pr9k8hi27qd9cliwn2fa0kg0v9b61bblba9m7la2prbxibzb8z9";
        Cabal-syntax = hackage "3.16.0.0" "09c987i6mn4j8ib894wfvh397rqxcw0ylid8bgn3xfqpwiwar58j";
        hix = minimal;
        integration = fast (overrideAttrs (old: { hackage_data_dir = "${hackageServer}/datafiles"; }));
        hackage-server = force (source.root hackageServer);
        unicode-data = notest;
      };

      package-set.gen-overrides = false;

    };

    latest.packages = lib.mkForce ["hix"];
    lower.packages = lib.mkForce ["hix"];

  };

  internal.cabal-extra.default-extensions = ["StrictData"];

  internal.hixCli = {
    dev = true;
    # commit = "8d92716141f252642a45b0ff0d5468e28a0701c7";
    # sha256 = "05wp05hdls051xjavnkzppk494zigr9bp6pqhhlbydkrfc455fbz";
  };

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

      update-cli-version = util.app release.updateCliVersionScript;

    };

    legacyPackages = tests.legacyPackages;

  };

}
