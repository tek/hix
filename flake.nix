{
  description = "Haskell/Nix development build tools";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/b139e44d78c36c69bcbb825b20dbfa51e7738347;
    nixpkgs_ghc943.follows = "nixpkgs";
    nixpkgs_ghc925.follows = "nixpkgs";
    nixpkgs_ghc902.follows = "nixpkgs";
    nixpkgs_ghc8107.follows = "nixpkgs";
    nixpkgs_ghc884.url = github:NixOS/nixpkgs/c0e881852006b132236cbf0301bd1939bb50867e;
    nixpkgs_nmd.url = github:NixOS/nixpkgs/bc41b01dd7a9fdffd32d9b03806798797532a5fe;
    nixpkgs_doc.url = github:NixOS/nixpkgs/f012515eb23bcda6a18ee606b7071dd0b2740d66;
    flake-utils.url = github:numtide/flake-utils;
    obelisk = { url = github:tek/obelisk/ghc9; flake = false; };
    thax.url = github:tek/thax;
    nmd = { url = git+https://gitlab.com/rycee/nmd; flake = false; };
  };

  outputs = inputs: import ./default.nix inputs;
}
