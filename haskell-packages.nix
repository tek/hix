{
  base,
  compiler,
  pkgs,
  overrides ? {},
  packages ? {},
  cabal2nixOptions ? "",
  profiling ? true,
  localPackage ? null,
}:
let
  combined = import ./ghc-overrides.nix {
    inherit pkgs base overrides packages cabal2nixOptions profiling localPackage;
  };
in
  pkgs.haskell.packages.${compiler}.override { overrides = combined; }
