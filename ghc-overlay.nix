{
  base,
  compiler,
  overrides ? {},
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
    packages = super.haskell.packages // {
      ${compiler} = super.haskell.packages.${compiler}.override { overrides = combined; };
    };
  };
}
