{
  description = "Haskell/Nix development build tools";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/8b5b7def915305c7d4f5cf236c095bf898bc7995";
    thax.url = "github:tek/thax";
  };

  outputs = inputs: import ./default.nix inputs;
}
