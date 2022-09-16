{
  description = "Haskell Development Build Tools";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/c3e4dc4f6d23c7e41dda5ce98956c75c7779e2fa;
    nixpkgs_ghc942.url = github:nixos/nixpkgs/c3e4dc4f6d23c7e41dda5ce98956c75c7779e2fa;
    nixpkgs_ghc924.url = github:nixos/nixpkgs/c3e4dc4f6d23c7e41dda5ce98956c75c7779e2fa;
    nixpkgs_ghc902.url = github:NixOS/nixpkgs/c3e4dc4f6d23c7e41dda5ce98956c75c7779e2fa;
    nixpkgs_ghc8107.url = github:NixOS/nixpkgs/c3e4dc4f6d23c7e41dda5ce98956c75c7779e2fa;
    nixpkgs_ghc884.url = github:NixOS/nixpkgs/c0e881852006b132236cbf0301bd1939bb50867e;
    nixpkgs_nmd.url = github:NixOS/nixpkgs/bc41b01dd7a9fdffd32d9b03806798797532a5fe;
    flake-utils.url = github:numtide/flake-utils;
    obelisk = { url = github:tek/obelisk/ghc9; flake = false; };
    thax.url = github:tek/thax;
    nmd = { url = git+https://gitlab.com/rycee/nmd; flake = false; };
  };

  outputs = inputs: import ./default.nix inputs;
}
