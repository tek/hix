{
  base,
  pkgs,
  overrides ? [],
  packages ? {},
  cabal2nixOptions ? "",
  profiling ? true,
  localPackage ? null,
  localBenchmark ? true,
  localHaddock ? false,
  localProfiling ? false,
}:
with pkgs.lib;
let
  deps = import ./deps { inherit pkgs profiling; };
  hixlib = import ./lib.nix { inherit (pkgs) lib; };
  inherit (pkgs.haskell.lib) dontHaddock dontBenchmark disableLibraryProfiling;

  reduceWork = d:
  let
    had = (if localHaddock then id else dontHaddock);
    bench = (if localBenchmark then id else dontBenchmark);
    prof = (if localProfiling then id else disableLibraryProfiling);
  in 
    prof (had (bench d));

  mkLocalPackage =
    if localPackage == null
    then reduceWork
    else localPackage;

  local = ghc: n: p: mkLocalPackage (ghc.callCabal2nixWithOptions n (hixlib.packagePath base p) cabal2nixOptions {});

  projectPackages = self: _: builtins.mapAttrs (local self) packages;

in
  pkgs.lib.composeExtensions projectPackages (deps.compose overrides)
