{
  base,
  compiler,
  overrides ? {},
  packages ? {},
  cabal2nixOptions ? "",
  profiling ? true,
  overridesConfig ? {},
}:
self: super:
let
  inherit (self.lib.strings) hasPrefix;

  combined = import ./ghc-overrides.nix ({
    inherit base overrides packages cabal2nixOptions profiling;
    pkgs = self;
  } // overridesConfig);
in {
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      ${compiler} = super.haskell.packages.${compiler}.override { overrides = combined; };
    };
  };
}
