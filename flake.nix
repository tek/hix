{
  description = "Haskell/Nix development build tools";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/4f6ebb60e63f3ccd1024eb2f31e653d2fe03fa66";

  outputs = inputs: import ./default.nix inputs;
}
