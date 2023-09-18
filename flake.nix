{
  description = "Haskell/Nix development build tools";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/5148520bfab61f99fd25fb9ff7bfbb50dad3c9db";
    thax.url = "github:tek/thax";
  };

  outputs = inputs: import ./default.nix inputs;
}
