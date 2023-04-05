{
  description = "Haskell/Nix development build tools";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/b139e44d78c36c69bcbb825b20dbfa51e7738347;
    nixpkgs_internal.url = github:NixOS/nixpkgs/f012515eb23bcda6a18ee606b7071dd0b2740d66;
    flake-utils.url = github:numtide/flake-utils;
    thax.url = github:tek/thax;
  };

  outputs = inputs: import ./default.nix inputs;
}
