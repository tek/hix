{
  description = "Haskell/Nix development build tools";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/4f2bd72692b1aae0a6db485ed3a83d1a933c2cac";

  outputs = inputs: import ./default.nix inputs;
}
