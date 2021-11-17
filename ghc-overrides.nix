{
  base,
  pkgs,
  overrides ? [],
  packages ? {},
  cabal2nixOptions ? "",
  profiling ? true,
}:
let
  deps = import ./deps { inherit pkgs profiling; };
  hixlib = import ./lib.nix { inherit (pkgs) lib; };
  inherit (pkgs.haskell.lib) dontHaddock dontBenchmark disableLibraryProfiling;

  reduceWork = d: disableLibraryProfiling (dontHaddock (dontBenchmark d));

  local = ghc: n: p: reduceWork (ghc.callCabal2nixWithOptions n (hixlib.packagePath base p) cabal2nixOptions {});

  projectPackages = self: _: builtins.mapAttrs (local self) packages;

in
  pkgs.lib.composeExtensions projectPackages (deps.compose overrides)
