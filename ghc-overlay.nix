{
  base,
  overrides ? _: {},
  packages ? {},
  cabal2nixOptions ? "",
  profiling ? true,
}:
self: super:
let
  inherit (self.lib.strings) hasPrefix;

  combined = import ./ghc-overrides.nix {
    inherit base overrides packages cabal2nixOptions profiling;
    pkgs = self;
  };

  overlay = name: set:
  if hasPrefix "ghc" name then set.override { overrides = combined; } else set;
in {
  haskell = super.haskell // {
    packages = builtins.mapAttrs overlay super.haskell.packages // {
      integer-simple = builtins.mapAttrs overlay super.haskell.packages.integer-simple;
    };
  };
}
