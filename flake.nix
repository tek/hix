{
  description = "Haskell/Nix development build tools";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/e12483116b3b51a185a33a272bf351e357ba9a99";

  outputs = inputs: import ./default.nix inputs;
}
