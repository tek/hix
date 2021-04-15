{
  nixpkgs,
  system,
  compiler,
  overrides ? { ... }: _: _: {},
  packages ? {},
  cabal2nixOptions ? "",
  profiling ? false,
}:
nixpkgs {
  inherit system;
  overlays = [(import ./ghc-overlay.nix { inherit compiler overrides packages cabal2nixOptions profiling; })];
}
