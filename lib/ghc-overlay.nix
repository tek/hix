{ compiler, ... }@args:
final: prev: {
  hixPackages = prev.haskell.packages.${compiler}.override {
    overrides = import ./overrides.nix { inherit (prev) lib; } args;
  };
}
