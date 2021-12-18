{
  base,
  pkgs,
  overrides ? [],
  packages ? {},
  cabal2nixOptions ? "",
  profiling ? true,
  localPackage ? null,
}:
with pkgs.lib;
let
  deps = import ./deps { inherit pkgs profiling; };
  hixlib = import ./lib.nix { inherit (pkgs) lib; };
  inherit (pkgs.haskell.lib) dontHaddock dontBenchmark disableLibraryProfiling;

  withMin = d:
  d // { min = dontHaddock (dontBenchmark (disableLibraryProfiling d)); };

  mkLocalPackage =
    if localPackage == null
    then id
    else localPackage;

  local = ghc: n: p:
  withMin (mkLocalPackage (ghc.callCabal2nixWithOptions n (hixlib.packagePath base p) cabal2nixOptions {}));

  projectPackages = self: _: builtins.mapAttrs (local self) packages;

in
  pkgs.lib.composeExtensions projectPackages (deps.compose overrides)
