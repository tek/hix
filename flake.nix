{
  description = "Haskell Development Build Tools";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/842f900e73c7ce985218cc4f455e34d1d56475c1;
    nixpkgs8104.url = github:NixOS/nixpkgs/842f900e73c7ce985218cc4f455e34d1d56475c1;
    nixpkgs884.url = github:NixOS/nixpkgs/c0e881852006b132236cbf0301bd1939bb50867e;
    nixpkgs865.url = github:NixOS/nixpkgs/cfed29bfcb28259376713005d176a6f82951014a;
    easy-hls.url = github:jkachmar/easy-hls-nix;
    flake-utils.url = github:numtide/flake-utils;
    obelisk = {
      url = github:tek/obelisk/tryp;
      flake = false;
    };
    snap-core = {
      url = github:obsidiansystems/snap-core?ref=ts-expose-fileserve-internals;
      flake = false;
    };
    thax.url = github:tek/thax;
  };

  outputs = inputs: import ./main.nix inputs;
}
