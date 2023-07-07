{ inputs, hix }:

hix.pro ({config, lib, ...}: {
  compiler = "ghc92";

  hackage = {
    versionFile = "ops/version.nix";
    tag = false;
    formatTag = { name, version }: if name == null then version else "${name}-${version}";
  };

  overrides = {hackage, ...}: {
    exon = hackage "1.4.0.0" "1m4i3a14wip985ncblfy2ikcy7gw5rryj9z497ah218d1nmwj7rl";
    flatparse = hackage "0.4.0.2" "0saxwgwbzijgm9v5w9nx3npl28szpkyz97m4shn8yanxq7gsjnvg";
    incipit-base = hackage "0.5.0.0" "02fdppamn00m94xqi4zhm6sl1ndg6lhn24m74w24pq84h44mynl6";
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
    src = ../packages/hix;

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
      "aeson >= 2.0 && < 2.2"
      "casing ^>= 0.1.4"
      "exon ^>= 1.4"
      "extra ^>= 1.7"
      "filepattern ^>= 0.1"
      "generic-lens ^>= 2.2"
      "lens >= 5.1 && < 5.3"
      "lens-regex-pcre ^>= 1.1"
      "optparse-applicative ^>= 0.17"
      "path ^>= 0.9"
      "path-io ^>= 1.7"
      "random ^>= 1.2"
      "transformers"
      "unix"
    ];

    executable.enable = true;

    test.enable = true;
    test.dependencies = [
      "Cabal"
      "exon ^>= 1.4"
      "hedgehog >= 1.1 && < 1.3"
      "path ^>= 0.9"
      "path-io ^>= 1.7"
      "tasty ^>= 1.4"
      "tasty-hedgehog >= 1.3 && < 1.5"
      "transformers"
    ];

  };

  outputs = {

    packages = let
      pkgs = import inputs.nixpkgs { inherit (config) system; };
      docs = import ./doc/default.nix { inherit inputs pkgs; inherit (config.internal) hixUrl; };
    in {
      docs = docs.html;
    };

    apps = let
      tests = import ../test/default.nix { inherit (config) pkgs; };
      util = import ./with-config.nix { inherit config lib util; };
    in {

      test = {
        type = "app";
        program = "${tests.main}";
      };

      cli = {
        type = "app";
        program = "${config.outputs.packages.hix}/bin/hix";
      };

      new = let
        prog = util.bootstrapWithDynamicCli "hix-new" ''
        $exe new --hix-url '${config.internal.hixUrl}' "$@"
        nix run .#gen-cabal
        '';
      in {
        type = "app";
        program = "${prog}";
      };

      bootstrap = let
        prog = util.bootstrapWithDynamicCli "hix-bootstrap" ''
        $exe bootstrap --hix-url '${config.internal.hixUrl}' "$@"
        nix run .#gen-cabal
        '';
      in {
        type = "app";
        program = "${prog}";
      };

      release-nix = let
        release = import ./release.nix { inherit config lib util; };
      in {
        type = "app";
        program = "${release.nix}";
      };

      release-all = let
        release = import ./release.nix { inherit config lib util; };
      in {
        type = "app";
        program = "${release.all}";
      };

    };

  };

})
