{
  description = "Haskell/Nix development build tools";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/f4a33546bdb5f81bd6cceb1b3cb19667145fed83";
    thax.url = "github:tek/thax";
  };

  outputs = inputs: import ./default.nix inputs;
}
