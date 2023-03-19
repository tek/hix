{
  inputs = { nixpkgs.url = "github:nixos/nixpkgs/b139e44d78c36c69bcbb825b20dbfa51e7738347"; };
  outputs = {self, nixpkgs}: let
    pkgs = import nixpkgs { system = "x86_64-linux"; };
    parser = pkgs.haskell.packages.ghc925.callCabal2nix "parser" ./. {};
  in {
    packages.x86_64-linux.parser = parser;
    apps.x86_64-linux.greet = {
      type = "app";
      program = "${parser}/bin/parser";
    };
  };
}
