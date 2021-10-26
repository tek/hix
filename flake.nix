{
  description = "Haskell Development Build Tools";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/21a885616cadbe48c6cc717495e63f66ae9274d3;
    nixpkgs_ghc901.url = github:NixOS/nixpkgs/21a885616cadbe48c6cc717495e63f66ae9274d3;
    nixpkgs_ghc8107.url = github:NixOS/nixpkgs/21a885616cadbe48c6cc717495e63f66ae9274d3;
    nixpkgs_ghc884.url = github:NixOS/nixpkgs/c0e881852006b132236cbf0301bd1939bb50867e;
    nixpkgs_ghc865.url = github:NixOS/nixpkgs/cfed29bfcb28259376713005d176a6f82951014a;
    easy-hls.url = github:jkachmar/easy-hls-nix;
    flake-utils.url = github:numtide/flake-utils;
    obelisk = {
      url = github:tek/obelisk/tryp;
      flake = false;
    };
    thax.url = github:tek/thax;
  };

  outputs = inputs: import ./main.nix inputs;
}
