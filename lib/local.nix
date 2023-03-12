{ inputs, hix }:

hix.pro ({config, lib, ...}: {

  packages = { hix = ../packages/hix; };

  overrides = {hackage, ...}: {
    exon = hackage "1.4.0.0" "1m4i3a14wip985ncblfy2ikcy7gw5rryj9z497ah218d1nmwj7rl";
    flatparse = hackage "0.4.0.2" "0saxwgwbzijgm9v5w9nx3npl28szpkyz97m4shn8yanxq7gsjnvg";
    incipit-base = hackage "0.5.0.0" "02fdppamn00m94xqi4zhm6sl1ndg6lhn24m74w24pq84h44mynl6";
  };

  hpack.packages = import ../ops/hpack.nix { inherit config lib; };

  ghci = {
    preludePackage = "incipit-base";
    preludeModule = "IncipitBase";
  };

  outputs = {

    packages = let
      nmd_pkgs = import inputs.nixpkgs_nmd { inherit (config) system; };
      docs = import ./doc/default.nix { inherit inputs; pkgs = nmd_pkgs; };
    in {
      docs = docs.docs.html;
      man = docs.docs.manPages;
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
