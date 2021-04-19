{
  base,
  compiler,
  overrides ? _: {},
  packages ? {},
  cabal2nixOptions ? "",
  profiling ? true,
}:
self: super:
let
  combined = import ./ghc-overrides.nix {
    inherit base overrides packages cabal2nixOptions profiling;
    pkgs = self;
  };
in {
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      ${compiler} = super.haskell.packages.${compiler}.override { overrides = combined; };
    };
  };
}
