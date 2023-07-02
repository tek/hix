{
  description = "Haskell/Nix development build tools";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/7e63eed145566cca98158613f3700515b4009ce3";
    thax.url = "github:tek/thax";
  };

  outputs = inputs: import ./default.nix inputs;
}
