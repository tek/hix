{
  description = "Haskell/Nix development build tools";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

  outputs = inputs: import ./default.nix inputs;
}
