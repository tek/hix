{
  base,
  compiler,
  overrides ? {},
  packages ? {},
  cabal2nixOptions ? "",
  profiling ? true,
  localPackage ? null,
}:
self: super:
let
  inherit (self.lib.strings) hasPrefix;

  combined = import ./ghc-overrides.nix {
    inherit base overrides packages cabal2nixOptions profiling localPackage;
    pkgs = self;
  };
in {
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      ${compiler} = super.haskell.packages.${compiler}.override { overrides = combined; };
    };
  };
}
