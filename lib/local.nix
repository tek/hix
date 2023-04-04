{ inputs, hix }:

hix.pro ({config, lib, ...}: {

  overrides = {hackage, ...}: {
    exon = hackage "1.4.0.0" "1m4i3a14wip985ncblfy2ikcy7gw5rryj9z497ah218d1nmwj7rl";
    flatparse = hackage "0.4.0.2" "0saxwgwbzijgm9v5w9nx3npl28szpkyz97m4shn8yanxq7gsjnvg";
    incipit-base = hackage "0.5.0.0" "02fdppamn00m94xqi4zhm6sl1ndg6lhn24m74w24pq84h44mynl6";
  };

  packages = import ../ops/hpack.nix;

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

  envs.dev.ghc.compiler = "ghc925";

  compat.enable = false;

  outputs = {

    packages = let
      pkgs = import inputs.nixpkgs_internal { inherit (config) system; };
      docs = import ./doc/default.nix { inherit inputs pkgs; };
    in {
      docs = docs.html;
    };

    apps = let
      tests = import ../test/default.nix { inherit (config) pkgs; };
    in {
      test = {
        type = "app";
        program = "${tests.main}";
      };
    };

  };

})
