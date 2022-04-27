{
  description = "Haskell Development Build Tools";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/bc41b01dd7a9fdffd32d9b03806798797532a5fe;
    nixpkgs_ghc922.url = github:nixos/nixpkgs/bc41b01dd7a9fdffd32d9b03806798797532a5fe;
    nixpkgs_ghc902.url = github:NixOS/nixpkgs/46821ea01c8f54d2a20f5a503809abfc605269d7;
    nixpkgs_ghc901.url = github:NixOS/nixpkgs/45a3f9d7725c7e21b252c223676cc56fb2ed5d6d;
    nixpkgs_ghc8107.url = github:NixOS/nixpkgs/46821ea01c8f54d2a20f5a503809abfc605269d7;
    nixpkgs_ghc884.url = github:NixOS/nixpkgs/c0e881852006b132236cbf0301bd1939bb50867e;
    nixpkgs_ghc865.url = github:NixOS/nixpkgs/cfed29bfcb28259376713005d176a6f82951014a;
    easy-hls.url = github:jkachmar/easy-hls-nix;
    flake-utils.url = github:numtide/flake-utils;
    obelisk = { url = github:tek/obelisk/ghc9; flake = false; };
    thax.url = github:tek/thax;
    nmd = { url = git+https://gitlab.com/rycee/nmd; flake = false; };
  };

  outputs = inputs: import ./default.nix inputs;
}
