args: _: super: {
  hixPackages = import ./haskell-packages.nix (args // { pkgs = super; });
}
