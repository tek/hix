{
  description = "Haskell Development Build Tools";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/b72ad04a8a324697d3fb92e19cd840379a902813;
    nixpkgs901.url = github:NixOS/nixpkgs/8ecc61c91a596df7d3293603a9c2384190c1b89a;
    nixpkgs8104.url = github:NixOS/nixpkgs/8ecc61c91a596df7d3293603a9c2384190c1b89a;
    nixpkgs884.url = github:NixOS/nixpkgs/c0e881852006b132236cbf0301bd1939bb50867e;
    nixpkgs865.url = github:NixOS/nixpkgs/cfed29bfcb28259376713005d176a6f82951014a;
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
