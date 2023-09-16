{
  description = "Haskell/Nix development build tools";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/46688f8eb5cd6f1298d873d4d2b9cf245e09e88e";
    thax.url = "github:tek/thax";
  };

  outputs = inputs: import ./default.nix inputs;
}
